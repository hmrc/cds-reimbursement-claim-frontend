/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import cats.data.EitherT
import cats.implicits._
import org.scalamock.handlers.CallHandler2
import org.scalamock.handlers.CallHandler3
import org.scalamock.handlers.CallHandler4
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SortCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.BankAccountReputationService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait MockBankAccountReputationService {
  this: MockFactory =>

  lazy val mockBankAccountReputationService: BankAccountReputationService = mock[BankAccountReputationService]

  def mockBusinessReputation(
    bankAccountDetails: BankAccountDetails = BankAccountDetails(AccountName(""), SortCode(""), AccountNumber("")),
    response: Either[ConnectorError, BankAccountReputation]
  )(implicit
    ec: ExecutionContext
  ): CallHandler2[BankAccountDetails, HeaderCarrier, EitherT[Future, ConnectorError, BankAccountReputation]] =
    (mockBankAccountReputationService
      .getBusinessAccountReputation(_: BankAccountDetails)(_: HeaderCarrier))
      .expects(bankAccountDetails, *)
      .returning(EitherT.fromEither[Future](response))
      .once()

  def mockPersonalReputation(
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String],
    response: Either[ConnectorError, BankAccountReputation]
  )(implicit
    ec: ExecutionContext
  ): CallHandler3[BankAccountDetails, Option[String], HeaderCarrier, EitherT[
    Future,
    ConnectorError,
    BankAccountReputation
  ]] =
    (mockBankAccountReputationService
      .getPersonalAccountReputation(_: BankAccountDetails, _: Option[String])(_: HeaderCarrier))
      .expects(bankAccountDetails, postCode, *)
      .returning(EitherT.fromEither[Future](response))
      .once()

  def mockBankAccountReputation(
    bankAccountType: BankAccountType,
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String],
    response: Either[ConnectorError, BankAccountReputation]
  )(implicit
    ec: ExecutionContext
  ): CallHandler4[BankAccountType, BankAccountDetails, Option[String], HeaderCarrier, EitherT[
    Future,
    ConnectorError,
    BankAccountReputation
  ]] =
    (mockBankAccountReputationService
      .checkBankAccountReputation(_: BankAccountType, _: BankAccountDetails, _: Option[String])(_: HeaderCarrier))
      .expects(bankAccountType, bankAccountDetails, postCode, *)
      .returning(EitherT.fromEither[Future](response))
      .once()

}
