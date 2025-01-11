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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Gen

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsBusinessAssessRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsPersonalAssessRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsSubject
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.BusinessCompleteResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.PersonalCompleteResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationErrorResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import org.scalacheck.Arbitrary

object BankAccountReputationGen {

  lazy val genReputationResponse: Gen[ReputationResponse] = Gen.oneOf(
    ReputationResponse.Yes,
    ReputationResponse.No,
    ReputationResponse.Inapplicable,
    ReputationResponse.Indeterminate,
    ReputationResponse.Error
  )

  implicit lazy val arbitraryAddress: Arbitrary[BarsAddress] =
    GeneratorUtils.gen[BarsAddress]

  implicit lazy val arbitraryBarsSubject: Arbitrary[BarsSubject] =
    GeneratorUtils.gen[BarsSubject]

  implicit lazy val arbitraryBusinessAssessRequest: Arbitrary[BarsBusinessAssessRequest] =
    GeneratorUtils.gen[BarsBusinessAssessRequest]

  implicit lazy val arbitraryBarsPersonalAssessRequest: Arbitrary[BarsPersonalAssessRequest] =
    GeneratorUtils.gen[BarsPersonalAssessRequest]

  implicit lazy val arbitraryBusinessCompleteResponse: Arbitrary[BusinessCompleteResponse] =
    GeneratorUtils.gen[BusinessCompleteResponse]

  implicit lazy val arbitraryPersonalCompleteResponse: Arbitrary[PersonalCompleteResponse] =
    GeneratorUtils.gen[PersonalCompleteResponse]

  implicit lazy val arbitraryReputationErrorResponse: Arbitrary[ReputationErrorResponse] =
    GeneratorUtils.gen[ReputationErrorResponse]

  implicit lazy val arbitraryBankAccountReputation: Arbitrary[BankAccountReputation] =
    GeneratorUtils.gen[BankAccountReputation]
}
