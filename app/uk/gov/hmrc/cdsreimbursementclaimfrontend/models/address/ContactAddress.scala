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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address

import cats.Eq
import play.api.i18n.Messages
import play.api.libs.json.OFormat
import play.api.libs.json.Json

final case class ContactAddress(
  line1: String,
  line2: Option[String],
  line3: Option[String],
  line4: String,
  postcode: String,
  country: Country,
  addressHasChanged: Boolean = false
) {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def computeChanges(previous: Option[ContactAddress]): ContactAddress =
    previous.fold(this)(that =>
      this.copy(addressHasChanged =
        this.line1 != that.line1 ||
          this.line2 != that.line2 ||
          this.line3 != that.line3 ||
          this.postcode != that.postcode ||
          this.country != that.country
      )
    )
}

object ContactAddress {

  implicit class AddressOps(private val a: ContactAddress) extends AnyVal {

    def getAddressLines(implicit messages: Messages): List[String] = {
      val lines =
        List(
          Some(a.line1),
          a.line2,
          a.line3,
          Some(a.line4),
          Some(a.postcode),
          messages.translate(s"country.${a.country.code}", Seq.empty)
        )

      lines.collect { case Some(s) => s }
    }
  }

  implicit val addressFormat: OFormat[ContactAddress] = Json.using[Json.WithDefaultValues].format[ContactAddress]

  implicit val eq: Eq[ContactAddress] = Eq.fromUniversalEquals
}
