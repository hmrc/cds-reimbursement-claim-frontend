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
import cats.syntax.all._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.EnterReimbursementClaimController.enterReimbursementClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim.from
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, DutyType, Error, Reimbursement, TaxCode, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils.moneyMapping
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{reimbursement => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class EnterReimbursementClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  enterReimbursementClaimPage: pages.enter_reimbursement_claim
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
      Future.successful(Redirect(reimbursementRoutes.CheckReimbursementClaimController.showReimbursements()))

    def start(dutyAndTaxCode: (DutyType, TaxCode)): Future[Result] =
      Future.successful(
        Redirect(
          reimbursementRoutes.EnterReimbursementClaimController.enterClaim(
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
        Ok(
          enterReimbursementClaimPage(
            dutyType,
            dutyCode,
            maybeAnswer
              .map(_.value(dutyType)(dutyCode))
              .filter(!_.isUnclaimed)
              .foldLeft(enterReimbursementClaimForm)((form, answer) => form.fill(answer))
          )
        )
      }
    }

  def submitClaim(dutyType: DutyType, taxCode: TaxCode): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SelectedDutyTaxCodesReimbursementAnswer] { (fillingOutClaim, maybeAnswer) =>
        def updateClaim(answer: SelectedDutyTaxCodesReimbursementAnswer) =
          updateSession(sessionCache, request)(
            _.copy(journeyStatus =
              from(fillingOutClaim)(_.copy(selectedDutyTaxCodesReimbursementAnswer = answer.some)).some
            )
          )

        enterReimbursementClaimForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(enterReimbursementClaimPage(dutyType, taxCode, formWithErrors)),
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
                        reimbursementRoutes.EnterReimbursementClaimController.enterClaim(
                          dutyAndTaxCode._1,
                          dutyAndTaxCode._2
                        )
                      )
                    }
                    .getOrElse(Redirect(reimbursementRoutes.CheckReimbursementClaimController.showReimbursements()))
                )
          )
      }
    }
}

object EnterReimbursementClaimController {

  val enterReimbursementClaimKey: String = "enter-reimbursement-claim"

  val enterReimbursementClaimForm: Form[Reimbursement] = Form(
    enterReimbursementClaimKey ->
      mapping(
        s"amount-paid"           -> moneyMapping(13, 2, "error.invalid"),
        s"amount-should-of-paid" -> moneyMapping(13, 2, "error.invalid", allowZero = true)
      )(Reimbursement.apply)(Reimbursement.unapply)
        .verifying(
          "invalid.reimbursement-claim",
          _.isValid
        )
  )
}
