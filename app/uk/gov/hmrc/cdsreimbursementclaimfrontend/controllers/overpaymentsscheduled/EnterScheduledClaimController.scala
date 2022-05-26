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
import cats.syntax.all._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.FormError
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterScheduledClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim.from
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class EnterScheduledClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  enterScheduledClaimPage: pages.enter_scheduled_claim
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[SelectedDutyTaxCodesReimbursementAnswer] =
    _.selectedDutyTaxCodesReimbursementAnswer

  def iterate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    def redirectToSummaryPage: Future[Result] =
      Future.successful(Redirect(routes.CheckScheduledClaimController.showReimbursements()))

    def start(dutyAndTaxCode: (DutyType, TaxCode)): Future[Result] =
      Future.successful(
        Redirect(
          routes.EnterScheduledClaimController.enterClaim(
            dutyAndTaxCode._1,
            dutyAndTaxCode._2
          )
        )
      )

    withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (_, maybeAnswer) =>
      maybeAnswer.flatMap(_.findUnclaimedReimbursement).fold(redirectToSummaryPage) { dutyAndTaxCode =>
        start(dutyAndTaxCode)
      }
    }
  }

  def enterClaim(dutyType: DutyType, dutyCode: TaxCode): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (_, maybeAnswer) =>
        val postAction: Call = routes.EnterScheduledClaimController.submitClaim(dutyType, dutyCode)
        Ok(
          enterScheduledClaimPage(
            dutyType,
            dutyCode,
            maybeAnswer
              .map(_.value(dutyType)(dutyCode))
              .filter(!_.isUnclaimed)
              .foldLeft(enterScheduledClaimForm)((form, answer) => form.fill(answer)),
            postAction
          )
        )
      }
    }

  def submitClaim(dutyType: DutyType, taxCode: TaxCode): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (fillingOutClaim, maybeAnswer) =>
        val postAction: Call                                             = routes.EnterScheduledClaimController.submitClaim(dutyType, taxCode)
        def updateClaim(answer: SelectedDutyTaxCodesReimbursementAnswer) =
          updateSession(sessionCache, request)(
            _.copy(journeyStatus =
              from(fillingOutClaim)(_.copy(selectedDutyTaxCodesReimbursementAnswer = answer.some)).some
            )
          )

        enterScheduledClaimForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                enterScheduledClaimPage(dutyType, taxCode, redirectVerificationMessage(formWithErrors), postAction)
              ),
            claim =>
              EitherT
                .fromOption[Future](
                  maybeAnswer.flatMap(_.update(dutyType, taxCode, claim)),
                  Error("Could not find reimbursement to update")
                )
                .semiflatTap(updateClaim)
                .fold(
                  logAndDisplayError("Error updating reimbursement for scheduled journey: "),
                  _.findUnclaimedReimbursement
                    .map { dutyAndTaxCode =>
                      Redirect(
                        routes.EnterScheduledClaimController.enterClaim(
                          dutyAndTaxCode._1,
                          dutyAndTaxCode._2
                        )
                      )
                    }
                    .getOrElse(Redirect(routes.CheckScheduledClaimController.showReimbursements()))
                )
          )
      }
    }

  def redirectVerificationMessage(formWithErrors: Form[AmountPaidWithCorrect]): Form[AmountPaidWithCorrect] = {
    val errors: Seq[FormError] = formWithErrors.errors.map {
      case formError if formError.messages.contains("invalid.claim") =>
        formError.copy(key = s"${formError.key}.actual-amount")
      case formError                                                 => formError
    }
    formWithErrors.copy(errors = errors)
  }
}
