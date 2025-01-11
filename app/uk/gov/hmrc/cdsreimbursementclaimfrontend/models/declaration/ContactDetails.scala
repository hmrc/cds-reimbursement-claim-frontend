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

import cats.syntax.apply.*
import play.api.libs.json.Json
import play.api.libs.json.OFormat

final case class ContactDetails(
  contactName: Option[String],
  addressLine1: Option[String],
  addressLine2: Option[String],
  addressLine3: Option[String],
  addressLine4: Option[String],
  postalCode: Option[String],
  countryCode: Option[String],
  telephone: Option[String],
  emailAddress: Option[String]
) {

  def maybeEmailAddress: Option[String] =
    emailAddress
      .flatMap(s => if s.trim.isEmpty then None else Some(s))

  def showAddress: Option[String] =
    addressLine1 *> postalCode *> Some(
      Seq(addressLine1, addressLine2, addressLine3, addressLine4, postalCode)
        .flatten(Option.option2Iterable)
        .mkString(", ")
    )
}

object ContactDetails {
  implicit val format: OFormat[ContactDetails] = Json.format[ContactDetails]
}
