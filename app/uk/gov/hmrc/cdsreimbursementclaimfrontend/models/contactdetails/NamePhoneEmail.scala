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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails

import cats.Eq
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails

final case class NamePhoneEmail(
  name: Option[String] = None,
  phoneNumber: Option[PhoneNumber] = None,
  email: Option[Email] = None
) {
  def nonEmpty(): Boolean = name.isDefined || phoneNumber.isDefined || email.isDefined
}

object NamePhoneEmail {
  implicit val eq: Eq[NamePhoneEmail] = Eq.fromUniversalEquals

  def fromMrnContactDetails(mrnContactDetails: MrnContactDetails): NamePhoneEmail =
    NamePhoneEmail(
      Some(mrnContactDetails.fullName),
      mrnContactDetails.phoneNumber,
      Some(mrnContactDetails.emailAddress)
    )
}
