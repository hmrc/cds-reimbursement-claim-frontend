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
import play.api.i18n.Messages
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.TemporaryJourneyExtractor.withAnswersAndRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{AccountName, AccountNumber, BankAccountView, BankAccountDetails, BankAccountType, DraftClaim, Error, SortCode, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.function.Predicate
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

  implicit val dataExtractor: DraftC285Claim => Option[BankAccountDetails] = _.bankAccountDetailsAnswer

  private def continuePage(implicit request: RequestWithSessionData[AnyContent], journey: JourneyBindable): Call = {
    lazy val uploadEvidence   = fileUploadRoutes.SupportingEvidenceController.uploadSupportingEvidence(journey)
    lazy val checkYourAnswers = fileUploadRoutes.SupportingEvidenceController.checkYourAnswers(journey)

    val maybeEvidences = for {
      session   <- request.sessionData
      claim     <- session.journeyStatus.collect { case FillingOutClaim(_, _, draftClaim: DraftClaim) =>
                     draftClaim
                   }
      evidences <- claim.fold(_.supportingEvidencesAnswer)
    } yield evidences

    maybeEvidences.fold(uploadEvidence)(_ => checkYourAnswers)
  }

  def checkBankAccountDetails(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DisplayDeclaration] { (fillingOutClaim, declaration, router) =>
        val isChange = fillingOutClaim.draftClaim.fold(identity).bankAccountDetailsAnswer.nonEmpty
        isChange match {
          case true  =>
            fillingOutClaim.draftClaim.fold(identity).bankAccountDetailsAnswer match {
              case Some(bankAccountDetails: BankAccountDetails) =>
                Ok(
                  checkBankAccountDetailsPage(
                    BankAccountView(
                      bankAccountDetails.accountName.value,
                      bankAccountDetails.sortCode.value,
                      bankAccountDetails.accountNumber.value
                    ),
                    continuePage.url,
                    router
                  )
                )
              case None                                         => Redirect(router.nextPageForCheckBankAccountDetails())
            }
          case false =>
            val answers = declaration.flatMap(p => p.displayResponseDetail.maskedBankDetails)

            answers.fold(Redirect(router.nextPageForCheckBankAccountDetails())) { maskedBankDetails =>
              (maskedBankDetails.consigneeBankDetails, maskedBankDetails.declarantBankDetails) match {
                case (Some(cmbd), _)    =>
                  Ok(
                    checkBankAccountDetailsPage(
                      BankAccountView(cmbd.accountHolderName, cmbd.sortCode, cmbd.accountNumber),
                      continuePage.url,
                      router
                    )
                  )
                case (None, Some(dmbd)) =>
                  Ok(
                    checkBankAccountDetailsPage(
                      BankAccountView(dmbd.accountHolderName, dmbd.sortCode, dmbd.accountNumber),
                      continuePage.url,
                      router
                    )
                  )
                case (None, None)       =>
                  Redirect(router.nextPageForCheckBankAccountDetails())
              }
            }

        }
      }(_.displayDeclaration, request, journey)
    }

  def enterBankAccountDetails(implicit journey: JourneyBindable): Action[AnyContent]  = show(false)
  def changeBankAccountDetails(implicit journey: JourneyBindable): Action[AnyContent] = show(true)

  protected def show(
    isChange: Boolean
  )(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BankAccountDetails] { (_, answers, router) =>
        val bankDetailsForm =
          answers.toList.foldLeft(BankAccountController.enterBankDetailsForm)((form, answer) => form.fill(answer))
        Ok(enterBankAccountDetailsPage(bankDetailsForm, router, isChange))
      }
    }

  def enterBankAccountDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      submit(isAmend = false)
    }

  def changeBankAccountDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      submit(isAmend = true)
    }

  protected def submit(isAmend: Boolean)(implicit
    request: RequestWithSessionData[AnyContent],
    messages: Messages,
    viewConfig: ViewConfig,
    journeyBindable: JourneyBindable
  ): Future[Result] =
    withAnswersAndRoutes[BankAccountDetails] { (fillingOutClaim, _, router) =>
      fillingOutClaim.draftClaim.fold(identity).bankAccountTypeAnswer match {
        case None                               => Redirect(router.nextPageForEnterBankAccountDetails(false))
        case Some(bankAccount: BankAccountType) =>
          BankAccountController.enterBankDetailsForm
            .bindFromRequest()
            .fold(
              requestFormWithErrors =>
                BadRequest(
                  enterBankAccountDetailsPage(
                    requestFormWithErrors,
                    router,
                    isAmend
                  )
                ),
              bankAccountDetails => {
                val updatedJourney =
                  FillingOutClaim.of(fillingOutClaim)(_.copy(bankAccountDetailsAnswer = Some(bankAccountDetails)))

                (for {
                  _                  <- EitherT
                                          .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                                          .leftMap((_: Unit) => Error("could not update session"))
                  reputationResponse <- {
                    val barsAccount =
                      BarsAccount(bankAccountDetails.sortCode.value, bankAccountDetails.accountNumber.value)

                    if (bankAccount === BankAccountType.BusinessBankAccount) {
                      claimService.getBusinessAccountReputation(BarsBusinessAssessRequest(barsAccount, None))
                    } else {
                      val claimant    = fillingOutClaim.draftClaim.detailsRegisteredWithCds
                      val address     = BarsAddress(Nil, None, claimant.map(_.contactAddress.postcode))
                      val accountName = Some(bankAccountDetails.accountName.value)
                      val subject     = BarsSubject(None, accountName, None, None, None, address)
                      claimService.getPersonalAccountReputation(BarsPersonalAssessRequest(barsAccount, subject))
                    }
                  }
                } yield reputationResponse).fold(
                  logAndDisplayError("could not process bank account details: "),
                  reputationResponse =>
                    if (reputationResponse.otherError.isDefined) {
                      val errorKey = reputationResponse.otherError.map(_.code).getOrElse("account-does-not-exist")
                      val form     = BankAccountController.enterBankDetailsForm
                        .fill(bankAccountDetails)
                        .withError("enter-bank-details", s"error.$errorKey")
                      BadRequest(enterBankAccountDetailsPage(form, router, isAmend))
                    } else if (reputationResponse.accountNumberWithSortCodeIsValid =!= ReputationResponse.Yes) {
                      val form = BankAccountController.enterBankDetailsForm
                        .fill(bankAccountDetails)
                        .withError("enter-bank-details", "error.moc-check-failed")
                      BadRequest(enterBankAccountDetailsPage(form, router, isAmend))
                    } else if (reputationResponse.accountExists =!= Some(ReputationResponse.Yes)) {
                      val form = BankAccountController.enterBankDetailsForm
                        .fill(bankAccountDetails)
                        .withError("enter-bank-details", s"error.account-does-not-exist")
                      BadRequest(enterBankAccountDetailsPage(form, router, isAmend))
                    } else {
                      Redirect(router.nextPageForEnterBankAccountDetails(true))
                    }
                )
              }
            )
      }
    }

}

object BankAccountController {

  val numberRegex: Predicate[String] = "^\\d+$".r.pattern.asPredicate()
  val charRegex: Predicate[String]   = "\\D+".r.pattern.asPredicate()

  def lengthError(acNum: String): Boolean =
    if ((acNum.length < 6 || acNum.length > 8) && numberRegex.test(acNum)) false
    else if ((acNum.length >= 6 && acNum.length <= 8) && numberRegex.test(acNum)) true
    else true

  def invalidError(acNum: String): Boolean =
    if ((acNum.length >= 6 && acNum.length <= 8) && charRegex.test(acNum)) false
    else if ((acNum.length >= 6 && acNum.length <= 8) && numberRegex.test(acNum)) true
    else if ((acNum.length < 6 || acNum.length > 8) && charRegex.test(acNum)) false
    else if ((acNum.length < 6 || acNum.length > 8) && numberRegex.test(acNum)) true
    else true

  val accountNumberMapping: Mapping[AccountNumber] =
    nonEmptyText
      .transform[String](s => s.replaceAll("[-( )]+", ""), identity)
      .verifying("error.length", str => lengthError(str))
      .verifying("error.invalid", str => invalidError(str))
      .transform[AccountNumber](
        s => {
          val paddedNumber = s.reverse.padTo(8, '0').reverse
          AccountNumber(paddedNumber)
        },
        _.value
      )

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
      "enter-bank-details.account-name"   -> accountNameMapping,
      "enter-bank-details.sort-code"      -> sortCodeMapping,
      "enter-bank-details.account-number" -> accountNumberMapping
    )(BankAccountDetails.apply)(BankAccountDetails.unapply)
  )

}
