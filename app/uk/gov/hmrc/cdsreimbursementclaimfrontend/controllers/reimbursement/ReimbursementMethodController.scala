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
import cats.implicits.catsSyntaxEq
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.JourneyBindable
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.ReimbursementMethodController.reimbursementMethodForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.ReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.ReimbursementMethodAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.reimbursement.select_reimbursement_method
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import scala.concurrent.Future

class ReimbursementMethodController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  selectReimbursementMethod: select_reimbursement_method
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftC285Claim => Option[ReimbursementMethodAnswer] = _.reimbursementMethodAnswer

  def showReimbursementMethod(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ReimbursementMethodAnswer] { (_, answers) =>
        val filledForm = answers.fold(reimbursementMethodForm)(reimbursementMethodForm.fill)
        Future.successful(Ok(selectReimbursementMethod(filledForm)))
      }
    }

  def submitReimbursementMethod(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[ReimbursementMethodAnswer] { (fillingOutClaim, _) =>
      ReimbursementMethodController.reimbursementMethodForm
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(selectReimbursementMethod(formWithErrors))),
          answer => {
            val updatedJourney = FillingOutClaim.of(fillingOutClaim)(_.copy(reimbursementMethodAnswer = Some(answer)))
            EitherT
              .liftF(updateSession(sessionCache, request)(_.copy(journeyStatus = Some(updatedJourney))))
              .leftMap((_: Unit) => Error("could not update session"))
              .fold(
                logAndDisplayError("could not get reimbursement method selected "),
                _ =>
                  answer match {
                    case ReimbursementMethodAnswer.CurrentMonthAdjustment =>
                      Redirect(
                        fileUploadRoutes.SupportingEvidenceController.uploadSupportingEvidence(JourneyBindable.Single)
                      )
                    case ReimbursementMethodAnswer.BankAccountTransfer    =>
                      Redirect(claimsRoutes.BankAccountController.checkBankAccountDetails(JourneyBindable.Single))
                  }
              )
          }
        )
    }
  }
}

object ReimbursementMethodController {

  val reimbursementMethodKey: String = "reimbursement-method"

  val reimbursementMethodForm: Form[ReimbursementMethodAnswer] =
    Form(
      mapping(
        reimbursementMethodKey -> number
          .verifying("error.invalid", a => a === 0 || a === 1)
          .transform[ReimbursementMethodAnswer](
            value =>
              if (value === 0) CurrentMonthAdjustment
              else BankAccountTransfer,
            {
              case CurrentMonthAdjustment => 0
              case BankAccountTransfer    => 1
            }
          )
      )(identity)(Some(_))
    )
}
