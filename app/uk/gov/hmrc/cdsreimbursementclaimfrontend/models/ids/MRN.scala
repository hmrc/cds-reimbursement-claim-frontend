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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids

import play.api.libs.json.{Json, OFormat}

import java.util.Locale

final case class MRN(value: String)

object MRN {

  def changeToUpperCaseWithoutSpaces(maybeMrn: String): MRN =
    MRN(maybeMrn.toUpperCase(Locale.UK).replaceAll("\\s", ""))

  def isValid(maybeMrn: String): Boolean = {

    val mrnWithoutSpaces: String = maybeMrn.toUpperCase(Locale.UK).replaceAll("\\s", "")
    val regex                    = """\d{2}[a-zA-Z]{2}\w{13}\d"""
    val entryNumberRegex         = """\d{3}\d{6}[a-zA-Z]{1}\d{8}"""
    mrnWithoutSpaces.matches(regex) && !mrnWithoutSpaces.matches(entryNumberRegex)
  }

  implicit val format: OFormat[MRN] = Json.format[MRN]
}
