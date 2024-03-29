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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import play.api.libs.json.Json
import play.api.libs.json.OFormat

final case class DisplayResponseDetail(
  declarationId: String,
  acceptanceDate: String,
  declarantReferenceNumber: Option[String],
  securityReason: Option[String],
  btaDueDate: Option[String],
  procedureCode: String,
  btaSource: Option[String],
  declarantDetails: DeclarantDetails,
  consigneeDetails: Option[ConsigneeDetails],
  accountDetails: Option[List[AccountDetails]],
  bankDetails: Option[BankDetails],
  maskedBankDetails: Option[BankDetails],
  ndrcDetails: Option[List[NdrcDetails]],
  securityDetails: Option[List[SecurityDetails]] = None
)

object DisplayResponseDetail {
  implicit val format: OFormat[DisplayResponseDetail] = Json.format[DisplayResponseDetail]
}
