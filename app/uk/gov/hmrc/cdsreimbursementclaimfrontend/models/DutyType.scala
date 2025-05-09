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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.kernel.Eq
import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

sealed abstract class DutyType(val repr: String, val ordinal: Int) {
  def taxCodes: Seq[TaxCode]
}

object DutyType {

  def apply(value: String): DutyType =
    DutyTypes.findUnsafe(value)

  def unapply(dutyType: DutyType): Option[String] =
    Some(dutyType.repr)

  case object UkDuty extends DutyType("uk-duty", 0) {
    override def taxCodes: Seq[TaxCode] = TaxCodes.UK
  }

  case object EuDuty extends DutyType("eu-duty", 1) {
    override def taxCodes: Seq[TaxCode] = TaxCodes.EU
  }

  case object Excise extends DutyType("excise-duty", 2) {
    override def taxCodes: Seq[TaxCode] = TaxCodes.excise
  }

  val simpleDutyTypeFormat: Format[DutyType] =
    SimpleStringFormat[DutyType](
      repr =>
        DutyTypes
          .find(repr)
          .getOrElse(throw new Exception(s"Cannot parse duty type from the string [$repr]")),
      _.repr
    )

  implicit val dutyTypFormat: Format[DutyType] = simpleDutyTypeFormat
  implicit val dutyTypeEq: Eq[DutyType]        = Eq.fromUniversalEquals[DutyType]

  implicit val ordering: Ordering[DutyType] = Ordering.by(_.ordinal)
}
