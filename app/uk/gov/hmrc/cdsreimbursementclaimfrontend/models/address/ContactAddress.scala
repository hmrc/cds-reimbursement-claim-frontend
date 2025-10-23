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
import play.api.libs.json.Json
import play.api.libs.json.OFormat

import java.util.UUID
import scala.annotation.tailrec

/** Address model returned from the Address Lookup Frontend service */
final case class ContactAddress(
  line1: String,
  line2: Option[String],
  line3: Option[String],
  line4: String,
  postcode: String,
  country: Country,
  addressHasChanged: Boolean = false,
  addressId: Option[UUID] = None
) {

  @tailrec
  def overflowExcessCharacters(): ContactAddress =
    this match {
      case ca @ ContactAddress(line1, line2, _, _, _, _, _, _) if line1.length > 35 =>
        val (l1, l2) = overflow(line1, line2, 35)
        ca.copy(line1 = l1, line2 = l2).overflowExcessCharacters()

      case ca @ ContactAddress(_, Some(line2), line3, _, _, _, _, _) if line2.length > 35 =>
        val (l2, l3) = overflow(line2, line3, 35)
        ca.copy(line2 = Some(l2), line3 = l3).overflowExcessCharacters()

      case ca @ ContactAddress(_, _, Some(line3), _, _, _, _, _) if line3.length > 35 =>
        val (l3, _) = overflow(line3, None, 35)
        ca.copy(line3 = Some(l3))

      case other =>
        other
    }

  private def overflow(a: String, b: Option[String], maxLength: Int): (String, Option[String]) = {
    val i       = a.take(maxLength).lastIndexOf(" ") match {
      case i if i >= 0 => i
      case _           => maxLength - 1
    }
    val a1      = a.take(i + 1)
    val a1Check = a1.endsWith(" ")
    val a2      = a.drop(i + 1)
    val a2Check = a2.endsWith(".") || a2.endsWith(",") || a2.endsWith(";")
    (
      if a1Check then a1.dropRight(1) else a1,
      Some(a2 + b.map(l => if a2Check then s" $l" else s", $l").getOrElse(""))
    )
  }

  def removeRedundantInformation(): ContactAddress = {
    val (l1, l2) = line2 match {
      case Some(l2) if line1.trim().endsWith(l2.trim())   => (line1, None)
      case Some(l2) if l2.trim().startsWith(line1.trim()) => (l2, None)
      case _                                              => (line1, line2)
    }
    this.copy(
      line1 = l1.trim(),
      line2 = l2.map(_.trim()),
      line3 = line3.map(_.trim()),
      line4 = line4.trim(),
      postcode = postcode.trim()
    )
  }

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

  implicit val addressFormat: OFormat[ContactAddress] = Json.using[Json.WithDefaultValues].format[ContactAddress]

  implicit val eq: Eq[ContactAddress] = Eq.fromUniversalEquals
}
