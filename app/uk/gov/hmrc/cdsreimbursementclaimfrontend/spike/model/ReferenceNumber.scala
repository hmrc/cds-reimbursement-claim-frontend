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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.spike.model

import play.api.libs.json.{Json, OFormat}

sealed trait ReferenceNumber extends Product with Serializable {
  val value: String
}

object ReferenceNumber {

  final case class MRN(value: String) extends ReferenceNumber

  final case class EntryNumber(value: String) extends ReferenceNumber

  implicit class StringOps(val s: String) extends AnyVal {
    def isMrn: Boolean         = isValidAgainst("""\d{2}[a-zA-Z]{2}\w{13}\d""")
    def isEntryNumber: Boolean = isValidAgainst("""\d{3}\d{6}[a-zA-Z]{1}\d{8}""")

    private def isValidAgainst(regex: String) =
      s.toUpperCase.replaceAll("\\s", "").matches(regex)
  }

  implicit val mrnFormat: OFormat[MRN] =
    Json.format[MRN]

  implicit val entryNumberFormat: OFormat[EntryNumber] =
    Json.format[EntryNumber]
}
