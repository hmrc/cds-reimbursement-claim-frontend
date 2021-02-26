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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request

import play.api.libs.json.{Json, OFormat}

final case class BarsAddress(
  lines: List[String], // One to four lines; cumulative length must be between 1 and 140 characters.
  town: Option[String], // Must be between 1 and 35 characters long
  postcode: Option[String]
) // Must be between 5 and 8 characters long, all uppercase. The internal space character can be omitted.

object BarsAddress {
  implicit val format: OFormat[BarsAddress] = Json.format[BarsAddress]

  val emptyAddress: BarsAddress = BarsAddress(lines = List(" "), None, None)

  def apply(lines: List[String], town: Option[String], postcode: Option[String]): BarsAddress = {
    val validLines = if (lines.forall(_.isEmpty)) List(" ") else lines

    val addressLinesMaxLength = 140
    val (truncatedLines, _)   = validLines.foldLeft((List[String](), 0)) { case ((s, l), c) =>
      (s :+ (if ((l + c.length) > addressLinesMaxLength) c.take(addressLinesMaxLength - l) else c), l + c.length)
    }

    new BarsAddress(
      truncatedLines.filterNot(_.isEmpty).take(4),
      town.flatMap(t => if (t.isEmpty) None else Some(t.take(35))),
      postcode
    )
  }
}
