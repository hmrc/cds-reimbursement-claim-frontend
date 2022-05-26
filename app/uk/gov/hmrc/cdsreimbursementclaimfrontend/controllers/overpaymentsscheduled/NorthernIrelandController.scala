/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import cats.data.EitherT
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.northernIrelandForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, Error}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim.from
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.{No, Yes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.claim_northern_ireland
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class NorthernIrelandController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
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

  implicit val journey: JourneyBindable                   = JourneyBindable.Scheduled
  implicit val dataExtractor: DraftClaim => Option[YesNo] = _.whetherNorthernIrelandAnswer
  private val postAction: Call = OverpaymentsRoutes.NorthernIrelandController.show(journey)

  val show: Action[AnyContent] =
    authenticatedActionWithSessionData
      .async { implicit request =>
        withAnswers[YesNo] { (_, answer) =>
          val emptyForm  = northernIrelandForm
          val filledForm = answer.fold(emptyForm)(emptyForm.fill)
          Ok(northernIrelandAnswerPage(filledForm, postAction))
        }
      }

  val submit: Action[AnyContent] =
    authenticatedActionWithSessionData
      .async { implicit request =>
        withAnswersAndRoutes[YesNo] { (fillingOutClaim, previousAnswer, routes) =>
          import routes._

          northernIrelandForm
            .bindFromRequest()
            .fold(
              formWithErrors => BadRequest(northernIrelandAnswerPage(formWithErrors, postAction)),
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
                          OverpaymentsRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
                        )
                      )
                  )
              }
            )
        }
      }
}

object NorthernIrelandController {

  val dataKey: String = "claim-northern-ireland"

}
