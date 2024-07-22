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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response

import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation

sealed trait ReputationResult {
  def toCommonResponse(): BankAccountReputation
}

final case class BusinessCompleteResponse(
  accountNumberIsWellFormatted: ReputationResponse,
  accountExists: Option[ReputationResponse] = None,
  accountName: Option[String] = None,
  nameMatches: Option[ReputationResponse] = None
) extends ReputationResult {
  def toCommonResponse(): BankAccountReputation =
    BankAccountReputation(
      accountNumberIsWellFormatted,
      accountExists,
      None,
      accountName,
      nameMatches
    )
}

object BusinessCompleteResponse {
  implicit val businessCompleteResponseFormat: OFormat[BusinessCompleteResponse] = Json.format[BusinessCompleteResponse]
}

final case class PersonalCompleteResponse(
  accountNumberIsWellFormatted: ReputationResponse,
  accountExists: Option[ReputationResponse] = None,
  accountName: Option[String] = None,
  nameMatches: Option[ReputationResponse] = None
) extends ReputationResult {
  def toCommonResponse(): BankAccountReputation =
    BankAccountReputation(
      accountNumberIsWellFormatted,
      accountExists,
      None,
      accountName,
      nameMatches
    )
}

object PersonalCompleteResponse {
  implicit val personalCompleteResponseFormat: OFormat[PersonalCompleteResponse] = Json.format[PersonalCompleteResponse]
}
