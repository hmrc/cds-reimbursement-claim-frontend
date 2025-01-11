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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids

import cats.data.Validated
import cats.implicits.catsSyntaxOption
import cats.Eq
import cats.Id
import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN.validityRegex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

import java.util.Locale

final case class MRN private (value: String) {

  def isValid: Boolean = value matches validityRegex
}

object MRN {

  private val validityRegex = """\d{2}[a-zA-Z]{2}\w{13}\d"""

  val validator: Validator[Id, MRN] = (maybeMrn: Option[MRN]) =>
    maybeMrn.toValidNel[AnswerError](MissingAnswerError("MRN")).andThen { mrn =>
      Validated.condNel[AnswerError, MRN](mrn.isValid, mrn, IncorrectAnswerError("MRN", "Invalid"))
    }

  def apply(value: String): MRN = {
    val valueInUppercaseWithNoSpaces = value.toUpperCase(Locale.UK).replaceAll("\\s", "")
    new MRN(valueInUppercaseWithNoSpaces)
  }

  implicit val eq: Eq[MRN]         = Eq.fromUniversalEquals
  implicit val format: Format[MRN] = SimpleStringFormat(MRN(_), _.value)
}
