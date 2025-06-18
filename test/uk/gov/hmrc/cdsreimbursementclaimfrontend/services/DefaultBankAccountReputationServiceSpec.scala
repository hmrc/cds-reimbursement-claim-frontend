/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import cats.data.EitherT
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.test.Helpers.await
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.BankAccountReputationConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SortCode
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.test.Helpers.defaultAwaitTimeout
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ConnectorFailure
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType.Personal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsBusinessAssessRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsPersonalAssessRequest

class DefaultBankAccountReputationServiceSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with BeforeAndAfterEach {

  implicit val hc: HeaderCarrier                         = HeaderCarrier()
  lazy val mockConnector: BankAccountReputationConnector = mock[BankAccountReputationConnector]
  val testService                                        = new DefaultBankAccountReputationService(mockConnector)
  val testBankAccountDetails                             = BankAccountDetails(
    accountName = AccountName("Testname"),
    sortCode = SortCode("testSortCode"),
    accountNumber = AccountNumber("testAccountNumber")
  )
  val testBankAccountReputation                          = BankAccountReputation(accountNumberWithSortCodeIsValid = Yes)
  val confirmedBankAccountReputation                     =
    BankAccountReputation(accountNumberWithSortCodeIsValid = Yes, accountExists = Some(Yes), nameMatches = Some(Yes))
  val testConnectorFailure                               = ConnectorFailure("testError")

  override def beforeEach(): Unit =
    super.beforeEach()

  override def afterEach(): Unit =
    super.afterEach()

  "The DefaultBankAccountReputationService" when {

    "getBusinessAccountReputation" should {

      "return bank account reputation when connector call is successful" in {
        (mockConnector
          .getBusinessReputation(_: BarsBusinessAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Right(testBankAccountReputation)))

        val result         = await(testService.getBusinessAccountReputation(testBankAccountDetails).value)
        val expectedResult = Right(testBankAccountReputation)

        result shouldBe expectedResult
      }
    }

    "getPersonalAccountReputation" should {

      "return bank account reputation when connector call is successful" in {
        (mockConnector
          .getPersonalReputation(_: BarsPersonalAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Right(testBankAccountReputation)))

        val result         = await(testService.getPersonalAccountReputation(testBankAccountDetails, None).value)
        val expectedResult = Right(testBankAccountReputation)

        result shouldBe expectedResult
      }
    }

    "checkBankAccountReputation" should {
      "delegate to getPersonalAccountReputation when account type is Personal" in {
        (mockConnector
          .getPersonalReputation(_: BarsPersonalAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Right(testBankAccountReputation)))
        val testBankAccountType = BankAccountType.Personal
        val result              =
          await(testService.checkBankAccountReputation(testBankAccountType, testBankAccountDetails, None).value)
        val expectedResult      = Right(testBankAccountReputation)

        result shouldBe expectedResult
      }

      "delegate to getBusinessAccountReputation when account type is Business" in {
        (mockConnector
          .getBusinessReputation(_: BarsBusinessAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Right(testBankAccountReputation)))
        val testBankAccountType = BankAccountType.Business
        val result              =
          await(testService.checkBankAccountReputation(testBankAccountType, testBankAccountDetails, None).value)
        val expectedResult      = Right(testBankAccountReputation)

        result shouldBe expectedResult
      }
    }

    "checkBankAccountReputationV2" should {

      "return business reputation when business account check is confirmed" in {
        (mockConnector
          .getBusinessReputation(_: BarsBusinessAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Right(confirmedBankAccountReputation)))

        val result         = await(testService.checkBankAccountReputationV2(testBankAccountDetails, None).value)
        val expectedResult = Right(confirmedBankAccountReputation)

        result shouldBe expectedResult
      }

      "fallback to personal reputation check when business account check is not confirmed and personal is confirmed" in {
        val unconfirmedBusinessReputation =
          BankAccountReputation(accountNumberWithSortCodeIsValid = ReputationResponse.No)

        (mockConnector
          .getBusinessReputation(_: BarsBusinessAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Right(unconfirmedBusinessReputation)))

        (mockConnector
          .getPersonalReputation(_: BarsPersonalAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Right(confirmedBankAccountReputation)))

        val result         = await(testService.checkBankAccountReputationV2(testBankAccountDetails, None).value)
        val expectedResult = Right(confirmedBankAccountReputation)

        result shouldBe expectedResult
      }

      "return business reputation when both business and personal checks are not confirmed" in {
        val unconfirmedBusinessReputation =
          BankAccountReputation(accountNumberWithSortCodeIsValid = ReputationResponse.No)
        val unconfirmedPersonalReputation =
          BankAccountReputation(accountNumberWithSortCodeIsValid = ReputationResponse.No)

        (mockConnector
          .getBusinessReputation(_: BarsBusinessAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Right(unconfirmedBusinessReputation)))

        (mockConnector
          .getPersonalReputation(_: BarsPersonalAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Right(unconfirmedPersonalReputation)))

        val result         = await(testService.checkBankAccountReputationV2(testBankAccountDetails, None).value)
        val expectedResult = Right(unconfirmedBusinessReputation)

        result shouldBe expectedResult
      }

      "return a connector error when connector call is unsuccessful" in {
        val testConnectorFailure = ConnectorFailure("testError")
        (mockConnector
          .getBusinessReputation(_: BarsBusinessAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Left(testConnectorFailure)))

        val result         = await(testService.checkBankAccountReputationV2(testBankAccountDetails, None).value)
        val expectedResult = Left(testConnectorFailure)

        result shouldBe expectedResult
      }

      "return a connector error when connector call is unsuccessful after business reputation was checked" in {
        val unconfirmedBusinessReputation =
          BankAccountReputation(accountNumberWithSortCodeIsValid = ReputationResponse.No)
        (mockConnector
          .getBusinessReputation(_: BarsBusinessAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Right(unconfirmedBusinessReputation)))
        (mockConnector
          .getPersonalReputation(_: BarsPersonalAssessRequest)(_: HeaderCarrier))
          .expects(*, *)
          .returning(EitherT.fromEither[Future](Left(testConnectorFailure)))

        val result         = await(testService.checkBankAccountReputationV2(testBankAccountDetails, None).value)
        val expectedResult = Left(testConnectorFailure)

        result shouldBe expectedResult
      }
    }
  }
}
