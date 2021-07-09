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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.TemporaryJourneyExtractor._

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

  def selectNorthernIrelandClaim(): Action[AnyContent] = show(false)
  def changeNorthernIrelandClaim(): Action[AnyContent] = show(true)

  def show(isAmend: Boolean): Action[AnyContent] = (featureSwitch.NorthernIreland.hideIfNotEnabled andThen
    authenticatedActionWithSessionData).async { implicit request =>
    withAnswers[ClaimNorthernIrelandAnswer] { (_, answers) =>
      val emptyForm  = ClaimNorthernIrelandController.claimNorthernIrelandForm
      val filledForm = answers.fold(emptyForm)(emptyForm.fill)
      Ok(claimNorthernIrelandPage(filledForm, isAmend))
    }
  }

  def selectNorthernIrelandClaimSubmit(): Action[AnyContent] = submit(false)
  def changeNorthernIrelandClaimSubmit(): Action[AnyContent] = submit(true)

  def submit(isAmend: Boolean): Action[AnyContent] =
    (featureSwitch.NorthernIreland.hideIfNotEnabled andThen authenticatedActionWithSessionData).async {
      implicit request =>
        withAnswers[ClaimNorthernIrelandAnswer] { (fillingOutClaim, _) =>
          ClaimNorthernIrelandController.claimNorthernIrelandForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  claimNorthernIrelandPage(
                    formWithErrors,
                    isAmend
                  )
                ),
              formOk => {

                val newNiAnswer = fillingOutClaim.draftClaim
                  .fold(_.claimNorthernIrelandAnswer)

                val answerChanged = newNiAnswer.isEmpty || newNiAnswer.exists(n => n.value =!= formOk.value)

                val newDraftClaim  = fillingOutClaim.draftClaim.fold(_.copy(claimNorthernIrelandAnswer = Some(formOk)))
                val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                EitherT
                  .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                  .leftMap((_: Unit) => Error("could not update session"))
                  .fold(
                    e => {
                      logger.warn("could not capture select number of claims", e)
                      errorHandler.errorResult()
                    },
                    _ =>
                      isAmend match {
                        case true  =>
                          if (answerChanged)
                            Redirect(routes.SelectBasisForClaimController.selectBasisForClaim(extractJourney))
                          else Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
                        case false => Redirect(routes.SelectBasisForClaimController.selectBasisForClaim(extractJourney))
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
