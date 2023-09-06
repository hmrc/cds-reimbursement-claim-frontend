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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import play.api.libs.json.Format
import play.api.libs.json.Json
import play.api.libs.json.OFormat
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ContactDetailsSource.Unknown
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

final case class MrnContactDetails(
  fullName: String,
  emailAddress: Option[Email],
  phoneNumber: Option[PhoneNumber],
  emailAddressHasChanged: Boolean = false,
  nameHasChanged: Boolean = false,
  phoneNumberHasChanged: Boolean = false
//  source: ContactDetailsSource = Unknown
) {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def computeChanges(previous: Option[MrnContactDetails]): MrnContactDetails =
    previous.fold(this)(that =>
      this.copy(
        emailAddressHasChanged = this.emailAddress != that.emailAddress,
        nameHasChanged = this.fullName != that.fullName,
        phoneNumberHasChanged = this.phoneNumber != that.phoneNumber
      )
    )
}

object MrnContactDetails {

  def unapply3(contactDetails: MrnContactDetails): Option[(String, Option[Email], Option[PhoneNumber])] =
    Some((contactDetails.fullName, contactDetails.emailAddress, contactDetails.phoneNumber))

  implicit val equality: Eq[MrnContactDetails]    = Eq.fromUniversalEquals[MrnContactDetails]
  implicit val format: OFormat[MrnContactDetails] = Json.using[Json.WithDefaultValues].format[MrnContactDetails]
}
//
//sealed abstract class ContactDetailsSource(source: String) extends Product with Serializable {
//  override def toString: String = source
//}
//
//object ContactDetailsSource {
//  val values: Map[String, ContactDetailsSource]   = Map(
//    "Unknown"               -> Unknown,
//    "UserAnswer"            -> UserAnswer,
//    "Acc14DeclarantDetails" -> Acc14DeclarantDetails,
//    "Acc14ConsigneeDetails" -> Acc14ConsigneeDetails,
//    "SignedInUserDetails"   -> SignedInUserDetails
//  )
//  def apply(source: String): ContactDetailsSource = values(source)
//  case object Unknown extends ContactDetailsSource("Unknown")
//  case object UserAnswer extends ContactDetailsSource("UserAnswer")
//  case object Acc14DeclarantDetails extends ContactDetailsSource("Acc14DeclarantDetails")
//  case object Acc14ConsigneeDetails extends ContactDetailsSource("Acc14ConsigneeDetails")
//  case object SignedInUserDetails extends ContactDetailsSource("SignedInUserDetails")
//
//  implicit val taxCodeEq: Eq[ContactDetailsSource]         = Eq.fromUniversalEquals[ContactDetailsSource]
//  implicit val taxCodeFormat: Format[ContactDetailsSource] = SimpleStringFormat(ContactDetailsSource(_), _.toString)
//}
