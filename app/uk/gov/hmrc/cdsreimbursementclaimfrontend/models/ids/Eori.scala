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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids

import cats.Eq
import play.api.libs.functional.syntax.toInvariantFunctorOps
import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori.validityRegex

final case class Eori(value: String) extends AnyVal {

  def isValid: Boolean = value matches validityRegex
}

object Eori {

  private val validityRegex = """^[a-zA-Z]{2}[0-9]{12,15}$"""

  implicit val equality: Eq[Eori] = Eq.fromUniversalEquals[Eori]

  implicit val format: Format[Eori] = implicitly[Format[String]].inmap(Eori(_), _.value)
}
