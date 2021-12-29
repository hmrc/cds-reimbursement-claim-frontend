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
import com.google.inject.Inject
import play.api.data.Form
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.ClaimNorthernIrelandController.whetherNorthernIrelandClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim.from
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.claim_northern_ireland
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature

import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class ClaimNorthernIrelandController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  northernIrelandAnswerPage: claim_northern_ireland
)(implicit
  viewConfig: ViewConfig,
  ec: ExecutionContext,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents
) extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  implicit val dataExtractor: DraftClaim => Option[YesNo] = _.whetherNorthernIrelandAnswer

  def selectWhetherNorthernIrelandClaim(implicit journey: JourneyBindable): Action[AnyContent] =
    (featureSwitch.hideIfNotEnabled(Feature.NorthernIreland) andThen authenticatedActionWithSessionData)
      .async { implicit request =>
        withAnswers[YesNo] { (_, answer) =>
          val emptyForm  = whetherNorthernIrelandClaim
          val filledForm = answer.fold(emptyForm)(emptyForm.fill)
          Ok(northernIrelandAnswerPage(filledForm))
        }
      }

  def selectWhetherNorthernIrelandClaimSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    (featureSwitch.hideIfNotEnabled(Feature.NorthernIreland) andThen authenticatedActionWithSessionData)
      .async { implicit request =>
        withAnswersAndRoutes[YesNo] { (fillingOutClaim, previousAnswer, routes) =>
          import routes._

          whetherNorthernIrelandClaim
            .bindFromRequest()
            .fold(
              formWithErrors => BadRequest(northernIrelandAnswerPage(formWithErrors)),
              currentAnswer => {
                val updatedJourney = from(fillingOutClaim)(
                  _.copy(
                    whetherNorthernIrelandAnswer = Some(currentAnswer),
                    basisOfClaimAnswer = (previousAnswer, currentAnswer) match {
                      case (Some(No), Yes)                                                              => None
                      case (Some(Yes), No) if fillingOutClaim.draftClaim.hasNorthernIrelandBasisOfClaim => None
                      case _                                                                            =>
                        fillingOutClaim.draftClaim.basisOfClaimAnswer
                    }
                  )
                )

                EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                  .leftMap(_ => Error("could not update session"))
                  .fold(
                    logAndDisplayError("could not capture select number of claims"),
                    _ =>
                      Redirect(
                        CheckAnswers.when(updatedJourney.draftClaim.isComplete)(alternatively =
                          claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
                        )
                      )
                  )
              }
            )
        }
      }
}

object ClaimNorthernIrelandController {

  val dataKey: String = "claim-northern-ireland"

  val whetherNorthernIrelandClaim: Form[YesNo] = YesOrNoQuestionForm(dataKey)
}
