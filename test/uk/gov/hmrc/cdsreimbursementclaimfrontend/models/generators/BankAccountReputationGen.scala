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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.{BarsAddress, BarsBusinessAssessRequest, BarsPersonalAssessRequest, BarsSubject}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.{BusinessCompleteResponse, CommonBarsResponse, PersonalCompleteResponse, ReputationErrorResponse}
import org.scalacheck.ScalacheckShapeless._

object BankAccountReputationGen {

  implicit val arbitraryAddress: Typeclass[BarsAddress]                                 = gen[BarsAddress]
  implicit val arbitraryBarsSubject: Typeclass[BarsSubject]                             = gen[BarsSubject]
  implicit val arbitraryBusinessAssessRequest: Typeclass[BarsBusinessAssessRequest]     = gen[BarsBusinessAssessRequest]
  implicit val arbitraryBarsPersonalAssessRequest: Typeclass[BarsPersonalAssessRequest] = gen[BarsPersonalAssessRequest]
  implicit val arbitraryBusinessCompleteResponse: Typeclass[BusinessCompleteResponse]   = gen[BusinessCompleteResponse]
  implicit val arbitraryPersonalCompleteResponse: Typeclass[PersonalCompleteResponse]   = gen[PersonalCompleteResponse]
  implicit val arbitraryCommonBarsResponse: Typeclass[CommonBarsResponse]               = gen[CommonBarsResponse]
  implicit val arbitraryReputationErrorResponse: Typeclass[ReputationErrorResponse]     = gen[ReputationErrorResponse]
}
