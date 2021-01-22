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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.UUID

import play.api.libs.json.{Json, Writes}

/** @param overpaymentDeclarationDisplayRequest
  */
final case class DeclarationInfoRequest(
  overpaymentDeclarationDisplayRequest: OverpaymentDeclarationDisplayRequest
)

object DeclarationInfoRequest {

  val dateFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssz").withZone(ZoneId.systemDefault())

  def apply(declarationId: String): DeclarationInfoRequest =
    DeclarationInfoRequest(
      OverpaymentDeclarationDisplayRequest(
        RequestCommon("CDSPay", dateFormat.format(Instant.now()), UUID.randomUUID().toString),
        RequestDetail(declarationId, None)
      )
    )

  implicit val requestCommonReads: Writes[RequestCommon]                                               = Json.writes
  implicit val requestDetailReads: Writes[RequestDetail]                                               = Json.writes
  implicit val overpaymentDeclarationDisplayRequestReads: Writes[OverpaymentDeclarationDisplayRequest] = Json.writes
  implicit val declarationInfoRequestReads: Writes[DeclarationInfoRequest]                             = Json.writes
}

/** @param requestCommon
  * @param requestDetail
  */
final case class OverpaymentDeclarationDisplayRequest(
  requestCommon: RequestCommon,
  requestDetail: RequestDetail
)

/** @param originatingSystem The name of the application system that submitted the message.
  * @param receiptDate
  * @param acknowledgementReference Unique id created at source after a form is saved. Unique ID throughout the journey of a message -- stored in the ETMP system for search and retrieval purposes
  */
final case class RequestCommon(
  originatingSystem: String,
  receiptDate: String,
  acknowledgementReference: String
)

/** @param declarationId
  * @param securityReason Security Reason Codes Description:
  *                       "MDP"  Missing Document Preference
  *                       "MDL"  Missing Document License Quota
  *                       "ACS"  Account Sales
  *                       "CEP"  CAP Entry Price
  *                       "CSD"  CAP Safeguard Duties
  *                       "T24"  Temporary Admission (2 years Expiration)
  *                       "TA6"  Temporary Admission (6 months Expiration)
  *                       "TA3"  Temporary Admission (3 months Expiration)
  *                       "TA2"  Temporary Admission (2 months Expiration)
  *                       "IPR"  Inward Processing Relief
  *                       "OPR"  Outward Processing Relief
  *                       "ENU"  End-use (Authorisation by Declaration)
  *                       "RED"  Revenue Dispute
  *                       "MOD"  Manual Override Deposit
  *                       "MDC" Missing Document CSDR              "CRQ" Critical Quota
  *                       "PDD" Provisional Dumping Duties (both Anti-Dumping and Countervailing)
  */
final case class RequestDetail(
  declarationId: String,
  securityReason: Option[String]
)
