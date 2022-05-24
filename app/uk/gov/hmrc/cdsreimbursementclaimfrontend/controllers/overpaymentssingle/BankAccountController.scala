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
import cats.implicits._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.i18n.Messages
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.{extractRoutes, withAnswersAndRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{OverpaymentsRoutes, routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountDetails, BankAccountType, DraftClaim, upscan => _, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{BankAccountReputationService, ClaimService}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class BankAccountController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  val claimService: ClaimService,
  val bankAccountReputationService: BankAccountReputationService,
  checkBankAccountDetailsPage: pages.check_bank_account_details,
  enterBankAccountDetailsPage: pages.enter_bank_account_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  implicit val journey: JourneyBindable = JourneyBindable.Single
  implicit val dataExtractor: DraftClaim => Option[BankAccountDetails] = _.bankAccountDetailsAnswer

  def checkBankAccountDetails: Action[AnyContent] =
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
                  claimsRoutes.SelectBankAccountTypeController.selectBankAccountType(journey)
                )
              )
            )
          }
          .getOrElse {
            Future.successful(Redirect(claimsRoutes.SelectBankAccountTypeController.selectBankAccountType(journey)))
          }
      }
    }

  def enterBankAccountDetails(implicit journey: JourneyBindable): Action[AnyContent] =
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
  )(implicit request: RequestWithSessionData[AnyContent], journeyBindable: JourneyBindable): Result =
    error match {
      case e @ ServiceUnavailableError(_, _) =>
        logger.warn(s"could not contact bank account service: $e")
        Redirect(claimsRoutes.ServiceUnavailableController.unavailable(journeyBindable))
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
      EitherT.rightT[Future, Error](Unit)
    }

  private def processBankAccountReputation(
    bankAccountReputation: BankAccountReputation,
    bankAccountDetails: BankAccountDetails,
    router: ReimbursementRoutes
  )(implicit journeyBindable: JourneyBindable, request: Request[_], mesaages: Messages) =
    if (bankAccountReputation.otherError.isDefined) {
      val errorKey = bankAccountReputation.otherError.map(_.code).getOrElse("account-does-not-exist")
      val form     = enterBankDetailsForm
        .fill(bankAccountDetails)
        .withError("enter-bank-details", s"error.$errorKey")
      BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
    } else if (bankAccountReputation.accountNumberWithSortCodeIsValid === ReputationResponse.No) {
      val form = enterBankDetailsForm
        .withError("enter-bank-details", "error.moc-check-no")
      BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
    } else if (bankAccountReputation.accountNumberWithSortCodeIsValid =!= ReputationResponse.Yes) {
      val form = enterBankDetailsForm
        .withError("enter-bank-details", "error.moc-check-failed")
      BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
    } else if (bankAccountReputation.accountExists === Some(ReputationResponse.Error)) {
      val form = enterBankDetailsForm
        .fill(bankAccountDetails)
        .withError("enter-bank-details", s"error.account-exists-error")
      BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
    } else if (bankAccountReputation.accountExists =!= Some(ReputationResponse.Yes)) {
      val form = enterBankDetailsForm
        .withError("enter-bank-details", s"error.account-does-not-exist")
      BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
    } else {
      Redirect(claimsRoutes.BankAccountController.checkBankAccountDetails(journeyBindable))
    }

  def enterBankAccountDetailsSubmit(implicit journeyBindable: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withAnswersAndRoutes[BankAccountDetails] { (fillingOutClaim: FillingOutClaim, _, router) =>
        fillingOutClaim.draftClaim.bankAccountTypeAnswer match {
          case None =>
            Redirect(claimsRoutes.SelectBankAccountTypeController.selectBankAccountType(journeyBindable))

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
