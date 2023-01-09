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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpayments

import cats.data.EitherT
import play.api.data.FormBinding.Implicits.formBinding
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendHeaderCarrierProvider
import scala.concurrent.ExecutionContext

trait SelectBankAccountTypeMixin {
  self: SessionDataExtractor
    with FrontendHeaderCarrierProvider
    with SessionUpdates
    with WithAuthAndSessionDataAction
    with Logging =>

  implicit val dataExtractor: DraftClaim => Option[BankAccountType] = _.bankAccountTypeAnswer
  private val emptyForm                                             = Forms.bankAccountTypeForm

  val sessionStore: SessionCache
  val postAction: Call
  val nextPageAction: Call
  val selectBankAccountTypePage: pages.select_bank_account_type
  implicit val viewConfig: ViewConfig
  implicit val ec: ExecutionContext
  implicit val errorHandler: ErrorHandler

  def show(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[BankAccountType] { (_, answer) =>
      val filledForm = answer.fold(emptyForm)(emptyForm.fill)
      Ok(selectBankAccountTypePage(filledForm, postAction))
    }
  }

  def submit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[BankAccountType] { (fillingOutClaim, _) =>
      emptyForm
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(selectBankAccountTypePage(formWithErrors, postAction)),
          bankAccountType => {
            val updatedJourney =
              FillingOutClaim.from(fillingOutClaim)(_.copy(bankAccountTypeAnswer = Some(bankAccountType)))
            EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
              .leftMap(_ => Error("could not update session"))
              .fold(
                logAndDisplayError("could not capture bank account type answer"),
                _ => Redirect(nextPageAction)
              )
          }
        )
    }
  }
}
