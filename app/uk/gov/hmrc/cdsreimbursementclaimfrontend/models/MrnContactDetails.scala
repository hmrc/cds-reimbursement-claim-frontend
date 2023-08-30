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
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber

final case class MrnContactDetails(
  fullName: String,
  emailAddress: Option[Email],
  phoneNumber: Option[PhoneNumber],
  emailAddressHasChanged: Boolean = false,
  nameHasChanged: Boolean = false,
  phoneNumberHasChanged: Boolean = false
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
  implicit val format: OFormat[MrnContactDetails] = Json.format[MrnContactDetails]
}
