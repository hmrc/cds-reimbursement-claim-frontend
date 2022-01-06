/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response

import play.api.libs.json.Json
import play.api.libs.json.OFormat

sealed trait ReputationResult {
  def toCommonResponse(): CommonBarsResponse
}

final case class BusinessCompleteResponse(
  accountNumberWithSortCodeIsValid: ReputationResponse,
  sortCodeIsPresentOnEISCD: String,
  accountExists: Option[ReputationResponse] = None,
  companyNameMatches: Option[ReputationResponse],
  companyPostCodeMatches: Option[ReputationResponse],
  companyRegistrationNumberMatches: Option[ReputationResponse],
  nonStandardAccountDetailsRequiredForBacs: Option[ReputationResponse] = None,
  sortCodeBankName: Option[String] = None
) extends ReputationResult {
  def toCommonResponse(): CommonBarsResponse = CommonBarsResponse(accountNumberWithSortCodeIsValid, accountExists, None)
}

object BusinessCompleteResponse {
  implicit val businessCompleteResponseFormat: OFormat[BusinessCompleteResponse] = Json.format[BusinessCompleteResponse]
}

final case class PersonalCompleteResponse(
  accountNumberWithSortCodeIsValid: ReputationResponse,
  sortCodeIsPresentOnEISCD: String,
  accountExists: Option[ReputationResponse] = None,
  nameMatches: Option[ReputationResponse] = None,
  addressMatches: Option[ReputationResponse] = None,
  nonConsented: Option[ReputationResponse] = None,
  subjectHasDeceased: Option[ReputationResponse] = None,
  nonStandardAccountDetailsRequiredForBacs: Option[ReputationResponse] = None,
  sortCodeBankName: Option[String] = None
) extends ReputationResult {
  def toCommonResponse(): CommonBarsResponse = CommonBarsResponse(accountNumberWithSortCodeIsValid, accountExists, None)
}

object PersonalCompleteResponse {

  implicit val personalCompleteResponseFormat: OFormat[PersonalCompleteResponse] = Json.format[PersonalCompleteResponse]
}
