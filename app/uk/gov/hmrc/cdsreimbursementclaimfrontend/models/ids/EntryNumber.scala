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

final case class EntryNumber(value: String) extends AnyVal

object EntryNumber {

  def changeToUpperCaseWithoutSpaces(maybeEntryNumber: String): EntryNumber =
    EntryNumber(maybeEntryNumber.toUpperCase.replaceAll("\\s", ""))

  def isValid(maybeEntryNumber: String): Boolean = {

    val entryNumberWithoutSpaces: String = maybeEntryNumber.toUpperCase.replaceAll("\\s", "")
    val entryNumberFormat                = """\d{3}\d{6}[a-zA-Z]{1}\d{8}"""
    entryNumberWithoutSpaces.matches(entryNumberFormat)
  }

  implicit val format: OFormat[EntryNumber] = Json.format[EntryNumber]
}
