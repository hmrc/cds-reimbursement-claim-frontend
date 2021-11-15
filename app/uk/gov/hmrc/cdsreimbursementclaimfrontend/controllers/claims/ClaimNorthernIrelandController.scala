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
import cats.implicits.catsSyntaxEq
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import play.api.data.Form
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, SessionDataExtractor, SessionUpdates, YesOrNoQuestionForm}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim.from
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, Error}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.claim_northern_ireland
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

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
    (featureSwitch.NorthernIreland.hideIfNotEnabled andThen authenticatedActionWithSessionData).async {
      implicit request =>
        withAnswers[YesNo] { (_, answer) =>
          val emptyForm  = ClaimNorthernIrelandController.whetherNorthernIrelandClaim
          val filledForm = answer.fold(emptyForm)(emptyForm.fill)
          Ok(northernIrelandAnswerPage(filledForm))
        }
    }

  def selectWhetherNorthernIrelandClaimSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    (featureSwitch.NorthernIreland.hideIfNotEnabled andThen authenticatedActionWithSessionData).async {
      implicit request =>
        withAnswersAndRoutes[YesNo] { (fillingOutClaim, previousAnswer, routes) =>
          import routes._

          ClaimNorthernIrelandController.whetherNorthernIrelandClaim
            .bindFromRequest()
            .fold(
              formWithErrors => BadRequest(northernIrelandAnswerPage(formWithErrors)),
              answer => {
                val updatedJourney = from(fillingOutClaim)(_.copy(whetherNorthernIrelandAnswer = Some(answer)))

                EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                  .leftMap(_ => Error("could not update session"))
                  .fold(
                    logAndDisplayError("could not capture select number of claims"),
                    _ =>
                      Redirect {
                        val isAnswerChanged = previousAnswer.forall(_ =!= answer)
                        CheckAnswers.when(fillingOutClaim.draftClaim.isComplete && !isAnswerChanged)(alternatively =
                          claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
                        )
                      }
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
