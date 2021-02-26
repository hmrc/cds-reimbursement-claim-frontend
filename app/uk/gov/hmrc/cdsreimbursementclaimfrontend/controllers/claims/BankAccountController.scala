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
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms._
import play.api.data.format.Formatter
import play.api.data.{Form, FormError, Forms, Mapping}
import play.api.libs.json.{Json, OFormat}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetailsAnswer.{CompleteBankAccountDetailAnswer, IncompleteBankAccountDetailAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.{CommonBarsResponse, ReputationResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.MaskedBankDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccount, BankAccountDetailsAnswer, DraftClaim, Error, SessionData, SortCode, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.function.Predicate
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

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

  def checkBankAccountDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMaskedBankDetails { (_, _, answers) =>
      answers.fold(
        errorHandler.errorResult()
      )(maskedBankDetails =>
        (maskedBankDetails.consigneeBankDetails, maskedBankDetails.declarantBankDetails) match {
          case (Some(cmbd), _)    =>
            Ok(checkBankAccountDetailsPage(BankAccount(cmbd.accountHolderName, cmbd.sortCode, cmbd.accountNumber)))
          case (None, Some(dmbd)) =>
            Ok(checkBankAccountDetailsPage(BankAccount(dmbd.accountHolderName, dmbd.sortCode, dmbd.accountNumber)))
          case _                  => errorHandler.errorResult()
        }
      )
    }
  }

  private def withMaskedBankDetails(
    f: (
      SessionData,
      FillingOutClaim,
      Option[MaskedBankDetails]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              s,
              r @ FillingOutClaim(_, _, c: DraftClaim)
            )
          ) =>
        val maybeMaskedBankDetails: Option[MaskedBankDetails] =
          c.fold(_.displayDeclaration.flatMap(p => p.displayResponseDetail.maskedBankDetails))
        f(s, r, maybeMaskedBankDetails)
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def enterBankAccountDetails: Action[AnyContent] =
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
                    BankAccountController.enterBankDetailsForm
                  )
                )
            },
          ifComplete =>
            Ok(
              enterBankAccountDetailsPage(
                BankAccountController.enterBankDetailsForm.fill(
                  ifComplete.bankAccountDetails
                )
              )
            )
        )
      }
    }

  private def withBankAccountDetailsAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      BankAccountDetailsAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeClaimantDetailsAsIndividualAnswer: Option[BankAccountDetailsAnswer] = draftClaim.fold(
          _.bankAccountDetailsAnswer
        )
        maybeClaimantDetailsAsIndividualAnswer.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteBankAccountDetailAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def changeBankAccountDetails: Action[AnyContent] =
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
                    isAmend = true
                  )
                )
            },
          ifComplete =>
            Ok(
              enterBankAccountDetailsPage(
                BankAccountController.enterBankDetailsForm.fill(
                  ifComplete.bankAccountDetails
                ),
                isAmend = true
              )
            )
        )
      }
    }

  def enterBankAccountDetailsSubmit: Action[AnyContent] =
    changeBankAccountDetailsSubmit(false, fileUploadRoutes.SupportingEvidenceController.uploadSupportingEvidence())

  def changeBankAccountDetailsSubmit: Action[AnyContent] =
    changeBankAccountDetailsSubmit(true, routes.CheckYourAnswersAndSubmitController.checkAllAnswers())

  protected def changeBankAccountDetailsSubmit(isAmend: Boolean, redirectTo: Call): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
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
                _ =>
                  CompleteBankAccountDetailAnswer(
                    bankAccountDetails
                  ),
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
                  val isBusinessAccount = bankAccountDetails.isBusinessAccount.headOption
                    .map(a => if (a === 0) true else false)
                    .getOrElse(false)
                  isBusinessAccount match {
                    case true  =>
                      claimService
                        .getBusinessAccountReputation(BarsBusinessAssessRequest(barsAccount, None))
                        .map(bar => CommonBarsResponse(bar.accountNumberWithSortCodeIsValid, bar.accountExists))
                    case false =>
                      val claimant = fillingOutClaim.draftClaim.claimantDetailsAsIndividual
                      val address  = BarsAddress(
                        Nil,
                        None,
                        claimant.map(_.contactAddress.postcode)
                      )
                      val subject  = BarsSubject(
                        None,
                        Some(updatedAnswers.bankAccountDetails.accountName.value),
                        None,
                        None,
                        None,
                        address
                      )
                      claimService
                        .getPersonalAccountReputation(BarsPersonalAssessRequest(barsAccount, subject))
                        .map(bar => CommonBarsResponse(bar.accountNumberWithSortCodeIsValid, bar.accountExists))
                  }
                }
              } yield reputationResponse).fold(
                e => {
                  logger.warn("could not process bank account details: ", e)
                  errorHandler.errorResult()
                },
                reputationResponse =>
                  if (reputationResponse.accountNumberWithSortCodeIsValid === ReputationResponse.No) {
                    val form = BankAccountController.enterBankDetailsForm
                      .fill(bankAccountDetails)
                      .withGlobalError("enter-bank-details.error.moc-check-failed")
                    BadRequest(enterBankAccountDetailsPage(form, isAmend = isAmend))
                  } else if (reputationResponse.accountExists === Some(ReputationResponse.No)) {
                    val form = BankAccountController.enterBankDetailsForm
                      .fill(bankAccountDetails)
                      .withGlobalError("enter-bank-details.error.account-does-not-exists")
                    BadRequest(enterBankAccountDetailsPage(form, isAmend = isAmend))
                  } else {
                    Redirect(redirectTo)
                  }
              )
            }
          )
      }
    }

}

object BankAccountController {

  val isBusinessAccountMapping: Mapping[List[Int]] = {
    val isBusinessAccountFormatter: Formatter[Int] =
      new Formatter[Int] {

        override def bind(
          key: String,
          data: Map[String, String]
        ): Either[Seq[FormError], Int] =
          readValue(key, data, identity)
            .flatMap {
              case "0" => Right(0)
              case _   => Left(FormError(key, "error.invalid"))
            }
            .leftMap(Seq(_))

        override def unbind(
          key: String,
          value: Int
        ): Map[String, String] =
          Map(key -> value.toString)
      }

    mapping(
      "enter-bank-details.is-business-account" -> Forms
        .list(of(isBusinessAccountFormatter))
    )(identity)(Some(_))

  }
  val accountNumberRegex: Predicate[String]          = "^\\d{6,8}$".r.pattern.asPredicate()
  val accountNumberMapping: Mapping[AccountNumber]   =
    nonEmptyText
      .transform[AccountNumber](s => AccountNumber(s.replaceAllLiterally(" ", "")), _.value)
      .verifying("invalid", e => accountNumberRegex.test(e.value))
  val accountNameRegex: Predicate[String]            = """^[A-Za-z0-9\-',/& ]{1,40}$""".r.pattern.asPredicate()
  val accountNameMapping: Mapping[AccountName]       =
    nonEmptyText
      .transform[AccountName](s => AccountName(s.trim()), _.value)
      .verifying("invalid", e => accountNameRegex.test(e.value))
  val sortCodeMapping: Mapping[SortCode]             =
    mapping(
      "" -> of(
        sortCodeFormatter(
          "enter-bank-details-sort-code-1",
          "enter-bank-details-sort-code-2",
          "enter-bank-details-sort-code-3",
          "enter-bank-details-sort-code"
        )
      )
    )(SortCode(_))(d => Some(d.value))
  val enterBankDetailsForm: Form[BankAccountDetails] = Form(
    mapping(
      "enter-bank-details.account-name"   -> accountNameMapping,
      ""                                  -> isBusinessAccountMapping,
      ""                                  -> sortCodeMapping,
      "enter-bank-details.account-number" -> accountNumberMapping
    )(BankAccountDetails.apply)(BankAccountDetails.unapply)
  )

  def readValue[T](
    key: String,
    data: Map[String, String],
    f: String => T,
    requiredErrorArgs: Seq[String] = Seq.empty
  ): Either[FormError, T] =
    data
      .get(key)
      .map(_.trim())
      .filter(_.nonEmpty)
      .fold[Either[FormError, T]](Left(FormError(key, "error.required", requiredErrorArgs))) { stringValue =>
        Either
          .fromTry(Try(f(stringValue)))
          .leftMap(_ => FormError(key, "error.invalid"))
      }

  def sortCodeFormatter(
    sortCode1Key: String,
    sortCode2Key: String,
    sortCode3Key: String,
    sortCode: String
  ): Formatter[String] =
    new Formatter[String] {
      def sortCodeFieldStringValues(
        data: Map[String, String]
      ): Either[FormError, (String, String, String)] =
        List(sortCode1Key, sortCode2Key, sortCode3Key)
          .map(data.get(_).map(_.trim).filter(_.nonEmpty)) match {
          case Some(sc1) :: Some(sc2) :: Some(sc3) :: Nil =>
            Right((sc1, sc2, sc3))
          case None :: Some(_) :: Some(_) :: Nil          =>
            Left(FormError(sortCode1Key, "error.required"))
          case Some(_) :: None :: Some(_) :: Nil          =>
            Left(FormError(sortCode2Key, "error.required"))
          case Some(_) :: Some(_) :: None :: Nil          =>
            Left(FormError(sortCode3Key, "error.required"))
          case Some(_) :: None :: None :: Nil             =>
            Left(FormError(sortCode2Key, "error.sc1-and-sc1.required"))
          case None :: Some(_) :: None :: Nil             =>
            Left(FormError(sortCode1Key, "error.sc1-and-sc3.required"))
          case None :: None :: Some(_) :: Nil             =>
            Left(FormError(sortCode1Key, "error.sc1-and-sc2.required"))
          case _                                          =>
            Left(FormError(sortCode, "error.required"))
        }

      override def bind(
        key: String,
        data: Map[String, String]
      ): Either[Seq[FormError], String] = {
        val result = for {
          scFields <- sortCodeFieldStringValues(data)
        } yield scFields._1 + scFields._2 + scFields._3
        result.leftMap(Seq(_))
      }

      override def unbind(key: String, value: String): Map[String, String] =
        Map(
          sortCode1Key -> value.substring(0, 2),
          sortCode2Key -> value.substring(2, 4),
          sortCode3Key -> value.substring(4, 6)
        )
    }

  final case class AccountNumber(value: String) extends AnyVal

  final case class AccountName(value: String) extends AnyVal

  final case class BankAccountDetails(
    accountName: AccountName,
    isBusinessAccount: List[Int],
    sortCode: SortCode,
    accountNumber: AccountNumber
  )

  object AccountNumber {
    implicit val format: OFormat[AccountNumber] = Json.format[AccountNumber]
  }

  object AccountName {
    implicit val format: OFormat[AccountName] = Json.format[AccountName]
  }

  object BankAccountDetails {
    implicit val format: OFormat[BankAccountDetails] = Json.format[BankAccountDetails]
  }
}
