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

package uk.gov.hmrc.cdsreimbursementclaimfrontend

import cats.data.{NonEmptyList, ValidatedNel}
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.DutyType

package object models {

  // validation

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

  implicit val dutyTypeOrdering: DutyType.DutyTypeOrdering.type = DutyType.DutyTypeOrdering

  implicit class IntegerOps(val integer: Int) extends AnyVal {
    def ordinalNaming: String = Ordinal(integer)
  }

  object Ordinal {

    private val wordings = Array(
      "zero",
      "first",
      "second",
      "third",
      "forth",
      "fifth",
      "sixth",
      "seventh",
      "eighth",
      "ninth"
    )

    def apply(integer: Int): String = integer.abs match {
      case n if n < wordings.length =>
        val sign = if (integer < 0) "minus " else ""
        sign + wordings(n)
      case n if n > 20              =>
        n % 10 match {
          case 1 => s"${integer}st"
          case 2 => s"${integer}nd"
          case 3 => s"${integer}rd"
          case _ => s"${integer}th"
        }
      case _                        => s"${integer}th"
    }
  }
}
