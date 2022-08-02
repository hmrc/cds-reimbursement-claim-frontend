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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import cats.data.EitherT
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.reimbursementMethodForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.ReimbursementMethodController.reimbursementMethodKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ReimbursementMethodController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  selectReimbursementMethod: pages.select_reimbursement_method
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[ReimbursementMethodAnswer] = _.reimbursementMethodAnswer

  private val form = reimbursementMethodForm(reimbursementMethodKey)

  val show: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ReimbursementMethodAnswer] { (_, answers) =>
        val filledForm = answers.fold(form)(form.fill)
        Future.successful(
          Ok(selectReimbursementMethod(filledForm, routes.ReimbursementMethodController.submit))
        )
      }
    }

  val submit: Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[ReimbursementMethodAnswer] { (fillingOutClaim, _) =>
      form
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              BadRequest(
                selectReimbursementMethod(
                  formWithErrors,
                  routes.ReimbursementMethodController.submit
                )
              )
            ),
          answer => {
            val updatedJourney = FillingOutClaim.from(fillingOutClaim)(_.copy(reimbursementMethodAnswer = Some(answer)))
            EitherT(updateSession(sessionCache, request)(_.copy(journeyStatus = Some(updatedJourney))))
              .leftMap(_ => Error("could not update session"))
              .fold(
                logAndDisplayError("could not get reimbursement method selected "),
                _ =>
                  Redirect((answer, isAmend(fillingOutClaim)) match {
                    case (_, true)                                             =>
                      routes.CheckYourAnswersAndSubmitController.checkAllAnswers
                    case (ReimbursementMethodAnswer.CurrentMonthAdjustment, _) =>
                      routes.ChooseFileTypeController.show
                    case (ReimbursementMethodAnswer.BankAccountTransfer, _)    =>
                      routes.BankAccountController.checkBankAccountDetails
                  })
              )
          }
        )
    }
  }

  private def isAmend(fillingOutClaim: FillingOutClaim): Boolean =
    fillingOutClaim.draftClaim.supportingEvidencesAnswer.isDefined
}

object ReimbursementMethodController {

  val reimbursementMethodKey: String = "reimbursement-method"
}
