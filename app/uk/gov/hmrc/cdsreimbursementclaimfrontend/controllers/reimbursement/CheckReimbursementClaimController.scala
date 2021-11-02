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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.data.Form
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.CheckReimbursementClaimController.whetherDutiesCorrectForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates, YesOrNoQuestionForm}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim.from
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, SelectedDutyTaxCodesReimbursementAnswer, YesNo}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, Error}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{reimbursement => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class CheckReimbursementClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  checkReimbursementClaim: pages.check_reimbursement_claim
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftClaim => Option[SelectedDutyTaxCodesReimbursementAnswer] =
    _.selectedDutyTaxCodesReimbursementAnswer

  def showReimbursements(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (fillingOutClaim, maybeAnswer) =>
      implicit val routes: ReimbursementRoutes =
        extractRoutes(fillingOutClaim.draftClaim, Scheduled)

      def redirectToSelectDutiesPage: Future[Result] =
        Future.successful(Redirect(reimbursementRoutes.SelectDutyTypesController.showDutyTypes()))

      def loadPage(answer: SelectedDutyTaxCodesReimbursementAnswer): Future[Result] =
        Future.successful(
          Ok(checkReimbursementClaim(answer, whetherDutiesCorrectForm))
        )

      maybeAnswer.fold(redirectToSelectDutiesPage)(loadPage)
    }
  }

  def submitReimbursements(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (fillingOutClaim, maybeAnswer) =>
      implicit val routes: ReimbursementRoutes =
        extractRoutes(fillingOutClaim.draftClaim, Scheduled)

      def selectDuties: Future[Result] =
        Future.successful(Redirect(reimbursementRoutes.SelectDutyTypesController.showDutyTypes()))

      maybeAnswer.fold(selectDuties)(reimbursements =>
        whetherDutiesCorrectForm
          .bindFromRequest()
          .fold(
            formWithErrors => Future.successful(BadRequest(checkReimbursementClaim(reimbursements, formWithErrors))),
            {
              case Yes =>
                val updatedClaim = from(fillingOutClaim)(_.copy(claimsAnswer = ClaimsAnswer(reimbursements)))

                EitherT(updateSession(sessionCache, request)(_.copy(journeyStatus = updatedClaim.some)))
                  .leftMap(_ => Error("Could not update session"))
                  .fold(
                    logAndDisplayError("Could not update reimbursement claims: "),
                    _ => Redirect(claimsRoutes.BankAccountController.checkBankAccountDetails(Scheduled))
                  )
              case No  => selectDuties
            }
          )
      )
    }
  }
}

object CheckReimbursementClaimController {

  val checkClaimSummaryKey: String = "check-claim-summary"

  val whetherDutiesCorrectForm: Form[YesNo] = YesOrNoQuestionForm(checkClaimSummaryKey)
}
