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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.CheckScheduledClaimController.whetherDutiesCorrectForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.{routes => overpaymentsScheduledRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim.from
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimedReimbursementsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class CheckScheduledClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  val controllerComponents: MessagesControllerComponents,
  checkScheduledClaimPage: pages.check_scheduled_claim_summary
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftClaim => Option[SelectedDutyTaxCodesReimbursementAnswer] =
    _.selectedDutyTaxCodesReimbursementAnswer

  def showReimbursements(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (fillingOutClaim, maybeAnswer) =>
      val routes: ReimbursementRoutes     = extractRoutes(fillingOutClaim.draftClaim, Scheduled)
      implicit val subKey: Option[String] = routes.subKey

      def redirectToSelectDutiesPage: Future[Result] =
        Future.successful(Redirect(overpaymentsScheduledRoutes.SelectDutyTypesController.showDutyTypes))

      def loadPage(answer: SelectedDutyTaxCodesReimbursementAnswer): Future[Result] =
        Future.successful(
          Ok(checkScheduledClaimPage(answer, whetherDutiesCorrectForm))
        )

      maybeAnswer.fold(redirectToSelectDutiesPage)(loadPage)
    }
  }

  def submitReimbursements(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (fillingOutClaim, maybeAnswer) =>
      val routes: ReimbursementRoutes     = extractRoutes(fillingOutClaim.draftClaim, Scheduled)
      implicit val subKey: Option[String] = routes.subKey
      import routes._

      def selectDuties: Future[Result] =
        Future.successful(Redirect(overpaymentsScheduledRoutes.SelectDutyTypesController.showDutyTypes))

      maybeAnswer.fold(selectDuties)(reimbursements =>
        whetherDutiesCorrectForm
          .bindFromRequest()
          .fold(
            formWithErrors => Future.successful(BadRequest(checkScheduledClaimPage(reimbursements, formWithErrors))),
            {
              case Yes =>
                val updatedClaim =
                  from(fillingOutClaim)(
                    _.copy(claimedReimbursementsAnswer = ClaimedReimbursementsAnswer(reimbursements))
                  )

                EitherT(updateSession(sessionCache, request)(_.copy(journeyStatus = updatedClaim.some)))
                  .leftMap(_ => Error("Could not update session"))
                  .fold(
                    logAndDisplayError("Could not update reimbursement claims: "),
                    _ =>
                      Redirect(
                        CheckAnswers.when(fillingOutClaim.draftClaim.isComplete)(alternatively =
                          OverpaymentsRoutes.BankAccountController.checkBankAccountDetails(Scheduled)
                        )
                      )
                  )
              case No  => selectDuties
            }
          )
      )
    }
  }
}

object CheckScheduledClaimController {

  val checkClaimSummaryKey: String = "check-claim-summary"

  val whetherDutiesCorrectForm: Form[YesNo] = YesOrNoQuestionForm(checkClaimSummaryKey)
}
