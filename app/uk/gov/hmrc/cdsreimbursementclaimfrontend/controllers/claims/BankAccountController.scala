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
import cats.implicits.catsSyntaxEq
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms._
import play.api.data.{Form, Mapping}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.{extractRoutes, withAnswersAndRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.BankAccountController.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{AccountName, AccountNumber, BankAccountDetails, BankAccountType, DraftClaim, Error, SortCode, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging.LoggerOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.BadGatewayException
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
  checkBankAccountDetailsPage: pages.check_bank_account_details,
  enterBankAccountDetailsPage: pages.enter_bank_account_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[BankAccountDetails] = _.bankAccountDetailsAnswer

  def checkBankAccountDetails(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        implicit val router: ReimbursementRoutes = extractRoutes(fillingOutClaim.draftClaim, journey)
        import router._

        fillingOutClaim.draftClaim.findNonEmptyBankAccountDetails
          .map { bankAccountDetails =>
            Future.successful(
              Ok(
                checkBankAccountDetailsPage(
                  bankAccountDetails,
                  CheckAnswers.when(fillingOutClaim.draftClaim.isComplete)(alternatively =
                    fileUploadRoutes.SupportingEvidenceController.uploadSupportingEvidence(journey)
                  )
                )
              )
            )
          }
          .getOrElse {
            Future.successful(Redirect(claimRoutes.SelectBankAccountTypeController.selectBankAccountType(journey)))
          }
      }
    }

  def enterBankAccountDetails(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BankAccountDetails] { (_, answers, router) =>
        val bankDetailsForm =
          answers.toList.foldLeft(enterBankDetailsForm)((form, answer) => form.fill(answer))
        Ok(enterBankAccountDetailsPage(bankDetailsForm, router))
      }
    }

  def enterBankAccountDetailsSubmit(implicit journeyBindable: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BankAccountDetails] { (fillingOutClaim, _, router) =>
        fillingOutClaim.draftClaim.bankAccountTypeAnswer match {
          case None =>
            Redirect(claimRoutes.SelectBankAccountTypeController.selectBankAccountType(journeyBindable))

          case Some(bankAccount: BankAccountType) =>
            enterBankDetailsForm
              .bindFromRequest()
              .fold(
                requestFormWithErrors => BadRequest(enterBankAccountDetailsPage(requestFormWithErrors, router)),
                bankAccountDetails => {
                  val updatedJourney =
                    FillingOutClaim.from(fillingOutClaim)(_.copy(bankAccountDetailsAnswer = Some(bankAccountDetails)))

                  (for {
                    _                  <- EitherT
                                            .liftF(
                                              updateSession(sessionStore, request)(
                                                _.copy(journeyStatus = Some(updatedJourney))
                                              )
                                            )
                                            .leftMap((_: Unit) => Error("could not update session"))
                    reputationResponse <- {
                      val barsAccount =
                        BarsAccount(bankAccountDetails.sortCode.value, bankAccountDetails.accountNumber.value)

                      if (bankAccount === BankAccountType.BusinessBankAccount) {
                        claimService.getBusinessAccountReputation(BarsBusinessAssessRequest(barsAccount, None))
                      } else {
                        val postCode    = fillingOutClaim.draftClaim.extractEstablishmentAddress.flatMap(_.postalCode)
                        val address     = BarsAddress(Nil, None, postCode)
                        val accountName = Some(bankAccountDetails.accountName.value)
                        val subject     = BarsSubject(None, accountName, None, None, None, address)
                        claimService.getPersonalAccountReputation(BarsPersonalAssessRequest(barsAccount, subject))
                      }
                    }
                  } yield reputationResponse).fold(
                    {
                      case e @ Error(_, Some(_: BadGatewayException), _) =>
                        logger warn ("could not contact bank account service: ", e)
                        Redirect(routes.ServiceUnavailableController.unavailable(journeyBindable))
                      case e                                             =>
                        logAndDisplayError("could not process bank account details: ")(errorHandler, request)(e)
                    },
                    reputationResponse =>
                      if (reputationResponse.otherError.isDefined) {
                        val errorKey = reputationResponse.otherError.map(_.code).getOrElse("account-does-not-exist")
                        val form     = enterBankDetailsForm
                          .fill(bankAccountDetails)
                          .withError("enter-bank-details", s"error.$errorKey")
                        BadRequest(enterBankAccountDetailsPage(form, router))
                      } else if (reputationResponse.accountNumberWithSortCodeIsValid === ReputationResponse.No) {
                        val form = enterBankDetailsForm
                          .withError("enter-bank-details", "error.moc-check-no")
                        BadRequest(enterBankAccountDetailsPage(form, router))
                      } else if (reputationResponse.accountNumberWithSortCodeIsValid =!= ReputationResponse.Yes) {
                        val form = enterBankDetailsForm
                          .withError("enter-bank-details", "error.moc-check-failed")
                        BadRequest(enterBankAccountDetailsPage(form, router))
                      } else if (reputationResponse.accountExists === Some(ReputationResponse.Error)) {
                        val form = enterBankDetailsForm
                          .fill(bankAccountDetails)
                          .withError("enter-bank-details", s"error.account-exists-error")
                        BadRequest(enterBankAccountDetailsPage(form, router))
                      } else if (reputationResponse.accountExists =!= Some(ReputationResponse.Yes)) {
                        val form = enterBankDetailsForm
                          .withError("enter-bank-details", s"error.account-does-not-exist")
                        BadRequest(enterBankAccountDetailsPage(form, router))
                      } else {
                        Redirect(claimRoutes.BankAccountController.checkBankAccountDetails(journeyBindable))
                      }
                  )
                }
              )
        }
      }
    }
}

object BankAccountController {

  val accountNumberMapping: Mapping[AccountNumber] =
    nonEmptyText
      .transform[String](s => s.replaceAll("[-( )]+", ""), identity)
      .verifying("error.length", str => AccountNumber.hasValidLength(str))
      .verifying("error.invalid", str => AccountNumber.isValid(str))
      .transform[AccountNumber](
        s => {
          val paddedNumber = s.reverse.padTo(8, '0').reverse
          AccountNumber(paddedNumber)
        },
        _.value
      )

  val sortCodeMapping: Mapping[SortCode] =
    nonEmptyText
      .transform[SortCode](s => SortCode(s.replaceAll("[-( )]+", "")), _.value)
      .verifying("invalid", e => SortCode.isValid(e.value))

  val accountNameMapping: Mapping[AccountName] =
    nonEmptyText(maxLength = 40)
      .transform[AccountName](s => AccountName(s.trim()), _.value)
      .verifying("invalid", e => AccountName.isValid(e.value))

  val enterBankDetailsForm: Form[BankAccountDetails] = Form(
    mapping(
      "enter-bank-details.account-name"   -> accountNameMapping,
      "enter-bank-details.sort-code"      -> sortCodeMapping,
      "enter-bank-details.account-number" -> accountNumberMapping
    )(BankAccountDetails.apply)(BankAccountDetails.unapply)
  )
}
