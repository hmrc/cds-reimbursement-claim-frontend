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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetailsAnswers.{CompleteBankAccountDetailAnswers, IncompleteBankAccountDetailAnswers}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.MaskedBankDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccount, BankAccountDetailsAnswers, DraftClaim, Error, SessionData, SortCode, upscan => _}
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
  checkBankAccountDetailsPage: pages.check_bank_account_details,
  enterBankAccountDetailsPage: pages.enter_bank_account_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withBankAccountDetailsAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      BankAccountDetailsAnswers
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeClaimantDetailsAsIndividualAnswer: Option[BankAccountDetailsAnswers] = draftClaim.fold(
          _.bankAccountDetailsAnswers
        )
        maybeClaimantDetailsAsIndividualAnswer.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteBankAccountDetailAnswers.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
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
          c.fold(_.maybeDeclaration.flatMap(p => p.maskedBankDetails))
        f(s, r, maybeMaskedBankDetails)
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def checkBankAccountDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMaskedBankDetails { (_, _, answers) =>
      answers.fold(
        errorHandler.errorResult()
      )(maskedBankDetails =>
        (maskedBankDetails.consigneeBankDetails, maskedBankDetails.declarantBankDetails) match {
          case (Some(consigneeBankDetails), _)    =>
            Ok(
              checkBankAccountDetailsPage(
                BankAccount(
                  consigneeBankDetails.accountHolderName,
                  consigneeBankDetails.sortCode,
                  consigneeBankDetails.accountNumber
                )
              )
            )
          case (None, Some(declarantBankDetails)) =>
            Ok(
              checkBankAccountDetailsPage(
                BankAccount(
                  declarantBankDetails.accountHolderName,
                  declarantBankDetails.sortCode,
                  declarantBankDetails.accountNumber
                )
              )
            )
          case _                                  => errorHandler.errorResult()
        }
      )
    }
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

  def enterBankAccountDetailsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withBankAccountDetailsAnswers { (_, fillingOutClaim, answers) =>
        BankAccountController.enterBankDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterBankAccountDetailsPage(
                  requestFormWithErrors
                )
              ),
            bankAccountDetails => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteBankAccountDetailAnswers(
                    bankAccountDetails
                  ),
                complete => complete.copy(bankAccountDetails = bankAccountDetails)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(bankAccountDetailsAnswers = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not bank account details", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(fileUploadRoutes.SupportingEvidenceController.uploadSupportingEvidence())
              )
            }
          )
      }
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

  def changeBankAccountDetailsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withBankAccountDetailsAnswers { (_, fillingOutClaim, answers) =>
        BankAccountController.enterBankDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterBankAccountDetailsPage(
                  requestFormWithErrors,
                  isAmend = true
                )
              ),
            bankAccountDetails => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteBankAccountDetailAnswers(
                    bankAccountDetails
                  ),
                complete => complete.copy(bankAccountDetails = bankAccountDetails)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(bankAccountDetailsAnswers = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not bank account details", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
              )
            }
          )
      }
    }

}

object BankAccountController {

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

  final case class AccountNumber(value: String) extends AnyVal

  object AccountNumber {
    implicit val format: OFormat[AccountNumber] = Json.format[AccountNumber]
  }

  val accountNumberRegex: Predicate[String] = "^\\d{6,8}$".r.pattern.asPredicate()

  val accountNumberMapping: Mapping[AccountNumber] =
    nonEmptyText
      .transform[AccountNumber](s => AccountNumber(s.replaceAllLiterally(" ", "")), _.value)
      .verifying("invalid", e => accountNumberRegex.test(e.value))

  final case class AccountName(value: String) extends AnyVal

  object AccountName {
    implicit val format: OFormat[AccountName] = Json.format[AccountName]
  }

  val accountNameRegex: Predicate[String] = """^[A-Za-z0-9\-',/& ]{1,40}$""".r.pattern.asPredicate()

  val accountNameMapping: Mapping[AccountName] =
    nonEmptyText
      .transform[AccountName](s => AccountName(s.trim()), _.value)
      .verifying("invalid", e => accountNameRegex.test(e.value))

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

  val sortCodeMapping: Mapping[SortCode] =
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

  final case class BankAccountDetails(
    accountName: AccountName,
    isBusinessAccount: List[Int],
    sortCode: SortCode,
    accountNumber: AccountNumber
  )

  object BankAccountDetails {
    implicit val format: OFormat[BankAccountDetails] = Json.format[BankAccountDetails]
  }
}
