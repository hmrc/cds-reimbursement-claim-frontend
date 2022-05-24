package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import cats.implicits._
import cats.implicits.catsSyntaxEq
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.extractRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.withAnswersAndRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

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

  implicit val dataExtractor: DraftClaim => Option[BankAccountDetails] = _.bankAccountDetailsAnswer
  
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
        Redirect(routes.ServiceUnavailableController.unavailable(journeyBindable))
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
      Redirect(claimRoutes.BankAccountController.checkBankAccountDetails(journeyBindable))
    }

  def enterBankAccountDetailsSubmit(implicit journeyBindable: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withAnswersAndRoutes[BankAccountDetails] { (fillingOutClaim: FillingOutClaim, _, router) =>
        fillingOutClaim.draftClaim.bankAccountTypeAnswer match {
          case None =>
            Redirect(OverpaymentsRoutes.SelectBankAccountTypeController.show(journeyBindable))

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