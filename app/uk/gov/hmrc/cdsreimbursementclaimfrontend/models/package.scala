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

package uk.gov.hmrc.cdsreimbursementclaimfrontend

import cats.data.NonEmptyList
import cats.data.ValidatedNel
import play.api.libs.json._

import java.text.NumberFormat
import java.util.Locale

package object models {

  type Total = Int

  // Validation

  type Validation[A] = ValidatedNel[String, A]

  // NEL format

  def nelReads[A : Reads]: Reads[NonEmptyList[A]] =
    Reads.of[List[A]].collect(JsonValidationError("Expected a non empty list but got an empty list")) { case x :: xs =>
      NonEmptyList(x, xs)
    }

  def nelWrites[A : Writes]: Writes[NonEmptyList[A]] =
    Writes.of[List[A]].contramap(_.toList)

  implicit def nelFormat[A : Format]: Format[NonEmptyList[A]] =
    Format(nelReads, nelWrites)

  // Utils

  private val currencyFormatter = NumberFormat.getCurrencyInstance(Locale.UK)

  implicit class BigDecimalOps(val amount: BigDecimal) extends AnyVal {
    def toPoundSterlingString: String =
      currencyFormatter.format(amount)
  }
}
