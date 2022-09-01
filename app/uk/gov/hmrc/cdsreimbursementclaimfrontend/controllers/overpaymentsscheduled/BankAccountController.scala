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
import cats.implicits._
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import play.api.i18n.Messages
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.extractRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.withAnswersAndRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountDetails, BankAccountType, DraftClaim, upscan => _, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_bank_account_details
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_bank_account_details
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class BankAccountController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val controllerComponents: MessagesControllerComponents,
  val config: Configuration,
  val claimService: ClaimService,
  val bankAccountReputationService: BankAccountReputationService,
  checkBankAccountDetailsPage: check_bank_account_details,
  enterBankAccountDetailsPage: enter_bank_account_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  implicit val journey: JourneyBindable                                = JourneyBindable.Scheduled
  implicit val dataExtractor: DraftClaim => Option[BankAccountDetails] = _.bankAccountDetailsAnswer

  val checkBankAccountDetails: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        implicit val router: ReimbursementRoutes = extractRoutes(fillingOutClaim.draftClaim, journey)
        implicit val subKey: Option[String]      = router.subKey
        import router._

        fillingOutClaim.draftClaim.findNonEmptyBankAccountDetails
          .map { bankAccountDetails =>
            Future.successful(
              Ok(
                checkBankAccountDetailsPage(
                  bankAccountDetails,
                  CheckAnswers.when(fillingOutClaim.draftClaim.isComplete)(alternatively =
                    OverpaymentsRoutes.ChooseFileTypeController.show(journey)
                  ),
                  OverpaymentsRoutes.SelectBankAccountTypeController.show(journey)
                )
              )
            )
          }
          .getOrElse {
            Future.successful(Redirect(OverpaymentsRoutes.SelectBankAccountTypeController.show(journey)))
          }
      }
    }

  val enterBankAccountDetails: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BankAccountDetails] { (_, answers, router) =>
        val bankDetailsForm =
          answers.toList.foldLeft(enterBankDetailsForm)((form, answer) => form.fill(answer))
        Ok(enterBankAccountDetailsPage(bankDetailsForm, router.submitUrlForEnterBankAccountDetails()))
      }
    }

  private def getBankAccountReputation(
    bankAccountType: BankAccountType,
    bankAccountDetails: BankAccountDetails,
    fillingOutClaim: FillingOutClaim
  )(implicit hc: HeaderCarrier): EitherT[Future, ConnectorError, BankAccountReputation] =
    if (bankAccountType === BankAccountType.Business) {
      bankAccountReputationService.getBusinessAccountReputation(bankAccountDetails)
    } else {
      val postCode = fillingOutClaim.draftClaim
        .extractEstablishmentAddress(fillingOutClaim.signedInUserDetails)
        .flatMap(_.postalCode)
      bankAccountReputationService.getPersonalAccountReputation(bankAccountDetails, postCode)
    }

  private def processCdsError[T : CdsError](
    error: T
  )(implicit request: RequestWithSessionData[AnyContent]): Result =
    error match {
      case e @ ServiceUnavailableError(_, _) =>
        logger.warn(s"could not contact bank account service: $e")
        Redirect(commonRoutes.ServiceUnavailableController.show())
      case e                                 =>
        logAndDisplayError("could not process bank account details: ", e)
    }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private def addBankAccount(
    bankAccountReputation: BankAccountReputation,
    request: RequestWithSessionData[AnyContent],
    updatedJourney: FillingOutClaim
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    if (bankAccountReputation.accountExists.contains(Yes)) {
      EitherT[Future, Error, Unit](
        updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney)))
      ).leftMap(_ => Error("could not update session"))
    } else {
      EitherT.rightT[Future, Error](())
    }

  private def processBankAccountReputation(
    bankAccountReputation: BankAccountReputation,
    bankAccountDetails: BankAccountDetails,
    router: ReimbursementRoutes
  )(implicit request: Request[_], mesaages: Messages): Result =
    if (bankAccountReputation.otherError.isDefined) {
      val errorKey = bankAccountReputation.otherError.map(_.code).getOrElse("account-does-not-exist")
      val form     = enterBankDetailsForm
        .fill(bankAccountDetails)
        .withError("enter-bank-account-details", s"error.$errorKey")
      BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
    } else if (bankAccountReputation.accountNumberWithSortCodeIsValid === ReputationResponse.No) {
      val form = enterBankDetailsForm
        .withError("enter-bank-account-details", "error.moc-check-no")
      BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
    } else if (bankAccountReputation.accountNumberWithSortCodeIsValid =!= ReputationResponse.Yes) {
      val form = enterBankDetailsForm
        .withError("enter-bank-account-details", "error.moc-check-failed")
      BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
    } else if (bankAccountReputation.accountExists === Some(ReputationResponse.Error)) {
      val form = enterBankDetailsForm
        .fill(bankAccountDetails)
        .withError("enter-bank-account-details", s"error.account-exists-error")
      BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
    } else if (bankAccountReputation.accountExists =!= Some(ReputationResponse.Yes)) {
      val form = enterBankDetailsForm
        .withError("enter-bank-account-details", s"error.account-does-not-exist")
      BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
    } else {
      Redirect(OverpaymentsRoutes.BankAccountController.checkBankAccountDetails(journey))
    }

  val enterBankAccountDetailsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withAnswersAndRoutes[BankAccountDetails] { (fillingOutClaim: FillingOutClaim, _, router) =>
        fillingOutClaim.draftClaim.bankAccountTypeAnswer match {
          case None =>
            Redirect(OverpaymentsRoutes.SelectBankAccountTypeController.show(JourneyBindable.Scheduled))

          case Some(bankAccount: BankAccountType) =>
            enterBankDetailsForm
              .bindFromRequest()
              .fold(
                requestFormWithErrors =>
                  BadRequest(
                    enterBankAccountDetailsPage(requestFormWithErrors, router.submitUrlForEnterBankAccountDetails())
                  ),
                bankAccountDetails => {
                  val updatedJourney: FillingOutClaim =
                    FillingOutClaim.from(fillingOutClaim)(_.copy(bankAccountDetailsAnswer = Some(bankAccountDetails)))

                  getBankAccountReputation(bankAccount, bankAccountDetails, fillingOutClaim).value.map {
                    case Left(error)                  =>
                      Future.successful(processCdsError(error))
                    case Right(bankAccountReputation) =>
                      addBankAccount(bankAccountReputation, request, updatedJourney).value.map {
                        case Left(error) =>
                          processCdsError(error)
                        case Right(_)    =>
                          processBankAccountReputation(bankAccountReputation, bankAccountDetails, router)
                      }
                  }.flatten
                }
              )
        }
      }
    }
}
