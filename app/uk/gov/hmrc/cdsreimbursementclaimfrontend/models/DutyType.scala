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

sealed abstract class DutyType(val repr: String, val taxCodes: Seq[TaxCode], val ordinal: Int)
    extends Product
    with Serializable

object DutyType {

  def apply(value: String): DutyType =
    DutyTypes.findUnsafe(value)

  def unapply(dutyType: DutyType): Option[String] =
    Some(dutyType.repr)

  case object UkDuty extends DutyType("uk-duty", TaxCodes.UK, 0)

  case object EuDuty extends DutyType("eu-duty", TaxCodes.EU, 1)

  case object Excise extends DutyType("excise-duty", TaxCodes.excise, 2)

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
