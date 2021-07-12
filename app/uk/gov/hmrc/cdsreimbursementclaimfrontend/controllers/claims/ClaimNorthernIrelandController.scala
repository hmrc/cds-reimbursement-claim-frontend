/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import cats.instances.future.catsStdInstancesForFuture
import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{ClaimNorthernIrelandAnswer, Error}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.claim_northern_ireland
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim

import javax.inject.Singleton
import scala.concurrent.ExecutionContext.Implicits.global

@Singleton
class ClaimNorthernIrelandController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  claimNorthernIrelandPage: claim_northern_ireland
)(implicit
  viewConfig: ViewConfig,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents
) extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[ClaimNorthernIrelandAnswer] = _.claimNorthernIrelandAnswer

  def selectNorthernIrelandClaim(implicit journey: JourneyBindable): Action[AnyContent] = show(isAmend = false)
  def changeNorthernIrelandClaim(implicit journey: JourneyBindable): Action[AnyContent] = show(isAmend = true)

  def show(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    (featureSwitch.NorthernIreland.hideIfNotEnabled andThen authenticatedActionWithSessionData).async {
      implicit request =>
        withAnswersAndRoutes[ClaimNorthernIrelandAnswer] { (_, answers, router) =>
          val emptyForm  = ClaimNorthernIrelandController.claimNorthernIrelandForm
          val filledForm = answers.fold(emptyForm)(emptyForm.fill)
          Ok(claimNorthernIrelandPage(filledForm, router, isAmend))
        }
    }

  def selectNorthernIrelandClaimSubmit(implicit journey: JourneyBindable): Action[AnyContent] = submit(isAmend = false)
  def changeNorthernIrelandClaimSubmit(implicit journey: JourneyBindable): Action[AnyContent] = submit(isAmend = true)

  def submit(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    (featureSwitch.NorthernIreland.hideIfNotEnabled andThen authenticatedActionWithSessionData).async {
      implicit request =>
        withAnswersAndRoutes[ClaimNorthernIrelandAnswer] { (fillingOutClaim, _, router) =>
          ClaimNorthernIrelandController.claimNorthernIrelandForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  claimNorthernIrelandPage(
                    formWithErrors,
                    router,
                    isAmend
                  )
                ),
              formOk => {

                val newNiAnswer    = fillingOutClaim.draftClaim.fold(_.claimNorthernIrelandAnswer)
                val answerChanged  = newNiAnswer.forall(n => n.value =!= formOk.value)
                val updatedJourney =
                  FillingOutClaim.of(fillingOutClaim)(_.copy(claimNorthernIrelandAnswer = Some(formOk)))

                EitherT
                  .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                  .leftMap((_: Unit) => Error("could not update session"))
                  .fold(
                    e => {
                      logger.warn("could not capture select number of claims", e)
                      errorHandler.errorResult()
                    },
                    _ =>
                      if (!isAmend) {
                        Redirect(routes.SelectBasisForClaimController.selectBasisForClaim(journey))
                      } else {
                        if (answerChanged) Redirect(routes.SelectBasisForClaimController.selectBasisForClaim(journey))
                        else Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
                      }
                  )
              }
            )
        }
    }
}

object ClaimNorthernIrelandController {

  import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimNorthernIrelandAnswer._

  val dataKey: String = "claim-northern-ireland"

  val claimNorthernIrelandForm: Form[ClaimNorthernIrelandAnswer] =
    Form(
      mapping(
        dataKey -> number
          .verifying("invalid", a => allClaimsTypes.map(_.value).contains(a))
          .transform[ClaimNorthernIrelandAnswer](allClaimsIntToType, allClaimsTypeToInt)
      )(identity)(Some(_))
    )

}
