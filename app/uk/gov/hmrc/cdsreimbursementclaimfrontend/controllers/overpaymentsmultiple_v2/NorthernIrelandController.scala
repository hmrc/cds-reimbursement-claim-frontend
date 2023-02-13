/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple_v2

import cats.data.EitherT
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.northernIrelandForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, JourneyControllerComponents, SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, Error}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim.from
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.{No, Yes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.claim_northern_ireland

import scala.concurrent.ExecutionContext

@Singleton
class NorthernIrelandController @Inject() (
  val jcc: JourneyControllerComponents,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  sessionStore: SessionCache,
  northernIrelandAnswerPage: claim_northern_ireland
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext, errorHandler: ErrorHandler)
  extends OverpaymentsMultipleJourneyBaseController
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {


  implicit val journey: JourneyBindable                   = JourneyBindable.Multiple
  implicit val dataExtractor: DraftClaim => Option[YesNo] = _.whetherNorthernIrelandAnswer
  private val postAction: Call                            = OverpaymentsRoutes.NorthernIrelandController.submit(journey)

  val show: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
        withAnswers[YesNo] { (_, answer) =>
          val emptyForm  = northernIrelandForm
          val filledForm = answer.fold(emptyForm)(emptyForm.fill)
          Ok(northernIrelandAnswerPage(filledForm, Some("multiple"), postAction)).asFuture
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
              formWithErrors => BadRequest(northernIrelandAnswerPage(formWithErrors, Some("multiple"), postAction)).asFuture,
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
