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
import com.google.inject.{Inject, Singleton}
import play.api.data.Forms.{mapping, number}
import play.api.data._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType.{allAccountTypes, allAccountsIntToType, allAccountsTypeToInt}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class SelectBankAccountTypeController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  selectBankAccountTypePage: pages.select_bank_account_type
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  implicit val dataExtractor: DraftClaim => Option[BankAccountType] = _.bankAccountTypeAnswer

  def selectBankAccountType(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BankAccountType] { (_, answer, router) =>
        val emptyForm  = SelectBankAccountTypeController.bankAccountTypeForm
        val filledForm = answer.fold(emptyForm)(emptyForm.fill)
        Ok(selectBankAccountTypePage(filledForm, router))
      }
    }

  def selectBankAccountTypeSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BankAccountType] { (fillingOutClaim, _, router) =>
        SelectBankAccountTypeController.bankAccountTypeForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                selectBankAccountTypePage(
                  formWithErrors,
                  router
                )
              ),
            formOk => {

              val updatedJourney = FillingOutClaim.from(fillingOutClaim)(_.copy(bankAccountTypeAnswer = Some(formOk)))

              EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap(_ => Error("could not update session"))
                .fold(
                  logAndDisplayError("could not capture bank account type answer"),
                  _ => Redirect(router.nextPageForSelectBankAccountType())
                )
            }
          )
      }
    }

}

object SelectBankAccountTypeController {

  val selectBankAccountTypeKey: String = "select-bank-account-type"

  val bankAccountTypeForm: Form[BankAccountType] =
    Form(
      mapping(
        selectBankAccountTypeKey -> number
          .verifying("invalid", a => allAccountTypes.map(_.value).contains(a))
          .transform[BankAccountType](allAccountsIntToType, allAccountsTypeToInt)
      )(identity)(Some(_))
    )

}
