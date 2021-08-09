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
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms._
import play.api.data.{Form, Mapping}
import play.api.i18n.Messages
import play.api.libs.json.{Json, OFormat}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetailsAnswer.{CompleteBankAccountDetailAnswer, IncompleteBankAccountDetailAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.MaskedBankDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountAcc14, BankAccountDetailsAnswer, DraftClaim, Error, SessionData, SortCode, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.function.Predicate
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class BankAccountController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val config: Configuration,
  val claimService: ClaimService,
  checkBankAccountDetailsPage: pages.check_bank_account_details,
  enterBankAccountDetailsPage: pages.enter_bank_account_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def continuePage(implicit request: RequestWithSessionData[AnyContent]): Call = {
    lazy val uploadEvidence   = fileUploadRoutes.SupportingEvidenceController.uploadSupportingEvidence()
    lazy val checkYourAnswers = fileUploadRoutes.SupportingEvidenceController.checkYourAnswers()

    val maybeEvidences = for {
      session   <- request.sessionData
      claim     <- session.journeyStatus.collect { case FillingOutClaim(_, _, draftClaim: DraftClaim) =>
                     draftClaim
                   }
      evidences <- claim.fold(_.supportingEvidencesAnswer)
    } yield evidences

    maybeEvidences.fold(uploadEvidence)(_ => checkYourAnswers)
  }

  private def withMaskedBankDetails(
    f: (SessionData, FillingOutClaim, Option[MaskedBankDetails]) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (s, r @ FillingOutClaim(_, _, c: DraftClaim)) =>
      val maybeMaskedBankDetails: Option[MaskedBankDetails] =
        c.fold(_.displayDeclaration.flatMap(p => p.displayResponseDetail.maskedBankDetails))
      f(s, r, maybeMaskedBankDetails)
    })

  private def withBankAccountDetailsAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      BankAccountDetailsAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (sessionData, fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)) =>
      val maybeClaimantDetailsAsIndividualAnswer: Option[BankAccountDetailsAnswer] = draftClaim.fold(
        _.bankAccountDetailsAnswer
      )
      maybeClaimantDetailsAsIndividualAnswer.fold[Future[Result]](
        f(sessionData, fillingOutClaim, IncompleteBankAccountDetailAnswer.empty)
      )(f(sessionData, fillingOutClaim, _))
    })

  def checkBankAccountDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMaskedBankDetails { (_, _, answers) =>
      answers.fold(Redirect(routes.BankAccountController.enterBankAccountDetails())) { maskedBankDetails =>
        (maskedBankDetails.consigneeBankDetails, maskedBankDetails.declarantBankDetails) match {
          case (Some(cmbd), _)    =>
            Ok(
              checkBankAccountDetailsPage(
                BankAccountAcc14(cmbd.accountHolderName, cmbd.sortCode, cmbd.accountNumber),
                continuePage.url
              )
            )
          case (None, Some(dmbd)) =>
            Ok(
              checkBankAccountDetailsPage(
                BankAccountAcc14(dmbd.accountHolderName, dmbd.sortCode, dmbd.accountNumber),
                continuePage.url
              )
            )
          case (None, None)       =>
            Redirect(routes.BankAccountController.enterBankAccountDetails())
        }
      }
    }
  }

  def enterBankAccountDetails(): Action[AnyContent]  = enterOrchangeBankAccountDetails(false)
  def changeBankAccountDetails(): Action[AnyContent] = enterOrchangeBankAccountDetails(true)

  protected def enterOrchangeBankAccountDetails(isAmend: Boolean): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withBankAccountDetailsAnswers { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.bankAccountDetails match {
              case Some(bankAccountDetails) =>
                Ok(enterBankAccountDetailsPage(BankAccountController.enterBankDetailsForm.fill(bankAccountDetails)))
              case None                     =>
                Ok(
                  enterBankAccountDetailsPage(
                    BankAccountController.enterBankDetailsForm,
                    isAmend = isAmend
                  )
                )
            },
          ifComplete =>
            Ok(
              enterBankAccountDetailsPage(
                BankAccountController.enterBankDetailsForm.fill(
                  ifComplete.bankAccountDetails
                ),
                isAmend = isAmend
              )
            )
        )
      }
    }

  def enterBankAccountDetailsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      enterOrchangeBankAccountDetailsSubmit(
        isAmend = false,
        continuePage
      )
    }

  def changeBankAccountDetailsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      enterOrchangeBankAccountDetailsSubmit(
        isAmend = true,
        routes.CheckYourAnswersAndSubmitController.checkAllAnswers()
      )
    }

  protected def enterOrchangeBankAccountDetailsSubmit(isAmend: Boolean, redirectTo: Call)(implicit
    request: RequestWithSessionData[_],
    messages: Messages,
    viewConfig: ViewConfig
  ): Future[Result] =
    withBankAccountDetailsAnswers { (_, fillingOutClaim, answers) =>
      BankAccountController.enterBankDetailsForm
        .bindFromRequest()
        .fold(
          requestFormWithErrors =>
            BadRequest(
              enterBankAccountDetailsPage(
                requestFormWithErrors,
                isAmend = isAmend
              )
            ),
          bankAccountDetails => {
            val updatedAnswers = answers.fold(
              _ => CompleteBankAccountDetailAnswer(bankAccountDetails),
              complete => complete.copy(bankAccountDetails = bankAccountDetails)
            )
            val newDraftClaim  =
              fillingOutClaim.draftClaim.fold(_.copy(bankAccountDetailsAnswer = Some(updatedAnswers)))

            val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

            (for {
              _                  <- EitherT
                                      .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                                      .leftMap((_: Unit) => Error("could not update session"))
              reputationResponse <- {
                val barsAccount       =
                  BarsAccount(bankAccountDetails.sortCode.value, bankAccountDetails.accountNumber.value)
                val isBusinessAccount = bankAccountDetails.isBusinessAccount.getOrElse(false)
                if (isBusinessAccount) {
                  claimService.getBusinessAccountReputation(BarsBusinessAssessRequest(barsAccount, None))
                } else {
                  val claimant    = fillingOutClaim.draftClaim.detailsRegisteredWithCds
                  val address     = BarsAddress(Nil, None, claimant.map(_.contactAddress.postcode))
                  val accountName = Some(updatedAnswers.bankAccountDetails.accountName.value)
                  val subject     = BarsSubject(None, accountName, None, None, None, address)
                  claimService.getPersonalAccountReputation(BarsPersonalAssessRequest(barsAccount, subject))
                }
              }
            } yield reputationResponse).fold(
              e => {
                logger.warn("could not process bank account details: ", e)
                errorHandler.errorResult()
              },
              reputationResponse =>
                if (reputationResponse.otherError.isDefined) {
                  val errorKey = reputationResponse.otherError.map(_.code).getOrElse("account-does-not-exist")
                  val form     = BankAccountController.enterBankDetailsForm
                    .fill(bankAccountDetails)
                    .withError("enter-bank-details", s"error.$errorKey")
                  BadRequest(enterBankAccountDetailsPage(form, isAmend = isAmend))
                } else if (reputationResponse.accountNumberWithSortCodeIsValid =!= ReputationResponse.Yes) {
                  val form = BankAccountController.enterBankDetailsForm
                    .fill(bankAccountDetails)
                    .withError("enter-bank-details", "error.moc-check-failed")
                  BadRequest(enterBankAccountDetailsPage(form, isAmend = isAmend))
                } else if (reputationResponse.accountExists =!= Some(ReputationResponse.Yes)) {
                  val form = BankAccountController.enterBankDetailsForm
                    .fill(bankAccountDetails)
                    .withError("enter-bank-details", s"error.account-does-not-exist")
                  BadRequest(enterBankAccountDetailsPage(form, isAmend = isAmend))
                } else {
                  Redirect(redirectTo)
                }
            )
          }
        )
    }

}

object BankAccountController {

  val accountNumberRegex: Predicate[String]        = "^\\d{8}$".r.pattern.asPredicate()
  val accountNumberMapping: Mapping[AccountNumber] =
    nonEmptyText(minLength = 6, maxLength = 8)
      .transform[AccountNumber](
        s => {
          val paddedNumber = s.reverse.padTo(8, '0').reverse
          AccountNumber(paddedNumber)
        },
        _.value
      )
      .verifying("invalid", e => accountNumberRegex.test(e.value))

  val sortCodeRegex: Predicate[String]   = "^\\d{6}$".r.pattern.asPredicate()
  val sortCodeMapping: Mapping[SortCode] =
    nonEmptyText
      .transform[SortCode](s => SortCode(s.replaceAll("[-( )]+", "")), _.value)
      .verifying("invalid", e => sortCodeRegex.test(e.value))

  val accountNameRegex: Predicate[String]      = """^[A-Za-z0-9\-',/& ]{1,40}$""".r.pattern.asPredicate()
  val accountNameMapping: Mapping[AccountName] =
    nonEmptyText(maxLength = 40)
      .transform[AccountName](s => AccountName(s.trim()), _.value)
      .verifying("invalid", e => accountNameRegex.test(e.value))

  val enterBankDetailsForm: Form[BankAccountDetails] = Form(
    mapping(
      "enter-bank-details.account-name"        -> accountNameMapping,
      "enter-bank-details.is-business-account" -> optional(boolean),
      "enter-bank-details.sort-code"           -> sortCodeMapping,
      "enter-bank-details.account-number"      -> accountNumberMapping
    )(BankAccountDetails.apply)(BankAccountDetails.unapply)
  )

  final case class AccountNumber(value: String) extends AnyVal

  final case class AccountName(value: String) extends AnyVal

  object AccountNumber {
    implicit val format: OFormat[AccountNumber] = Json.format[AccountNumber]
  }

  object AccountName {
    implicit val format: OFormat[AccountName] = Json.format[AccountName]
  }

}
