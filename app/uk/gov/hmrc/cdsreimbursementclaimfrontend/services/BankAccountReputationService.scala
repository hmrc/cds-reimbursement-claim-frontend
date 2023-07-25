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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import cats.data.EitherT
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.BankAccountReputationConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Singleton
import scala.concurrent.Future

@ImplementedBy(classOf[DefaultBankAccountReputationService])
trait BankAccountReputationService {
  def checkBankAccountReputation(
    bankAccountType: BankAccountType,
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit hc: HeaderCarrier): EitherT[Future, ConnectorError, BankAccountReputation]

  def getBusinessAccountReputation(
    bankAccountDetails: BankAccountDetails
  )(implicit hc: HeaderCarrier): EitherT[Future, ConnectorError, BankAccountReputation]

  def getPersonalAccountReputation(
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit hc: HeaderCarrier): EitherT[Future, ConnectorError, BankAccountReputation]
}

@Singleton
class DefaultBankAccountReputationService @Inject() (bankAccountReputationConnector: BankAccountReputationConnector)
    extends BankAccountReputationService
    with Logging {

  def checkBankAccountReputation(
    bankAccountType: BankAccountType,
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit hc: HeaderCarrier): EitherT[Future, ConnectorError, BankAccountReputation] =
    bankAccountType match {
      case BankAccountType.Personal =>
        getPersonalAccountReputation(bankAccountDetails, postCode)
      case BankAccountType.Business =>
        getBusinessAccountReputation(bankAccountDetails)
    }

  def getBusinessAccountReputation(
    bankAccountDetails: BankAccountDetails
  )(implicit hc: HeaderCarrier): EitherT[Future, ConnectorError, BankAccountReputation] = {
    val barsAccount: BarsAccount               =
      BarsAccount(bankAccountDetails.sortCode.value, bankAccountDetails.accountNumber.value)
    val accountName                            = bankAccountDetails.accountName.value
    val barsBusiness                           = Some(BarsBusiness(accountName, None))
    val barsRequest: BarsBusinessAssessRequest = BarsBusinessAssessRequest(barsAccount, barsBusiness)
    bankAccountReputationConnector.getBusinessReputation(barsRequest)
  }

  def getPersonalAccountReputation(
    bankAccountDetails: BankAccountDetails,
    postCode: Option[String]
  )(implicit hc: HeaderCarrier): EitherT[Future, ConnectorError, BankAccountReputation] = {
    val barsAccount                            = BarsAccount(bankAccountDetails.sortCode.value, bankAccountDetails.accountNumber.value)
    val accountName                            = Some(bankAccountDetails.accountName.value)
    val subject                                = BarsSubject(None, accountName, None, None, None, None)
    val barsRequest: BarsPersonalAssessRequest = BarsPersonalAssessRequest(barsAccount, subject)
    bankAccountReputationConnector.getPersonalReputation(barsRequest)
  }
}
