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

import cats.implicits.catsSyntaxEq
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.*

object TaxCodes {

  val UK: Seq[TaxCode] = List(A00, A20, A30, A35, A40, A45, B00)

  val ukTaxCodeSet: Set[TaxCode] = UK.toSet

  val EU: Seq[TaxCode] = List(A50, A70, A80, A85, A90, A95, B05)

  val euTaxCodeSet: Set[TaxCode] = EU.toSet

  val excise: Seq[TaxCode] = List(
    NI407,
    NI411,
    NI412,
    NI413,
    NI415,
    NI419,
    NI421,
    NI422,
    NI423,
    NI425,
    NI429,
    NI431,
    NI433,
    NI435,
    NI438,
    NI440,
    NI441,
    NI442,
    NI443,
    NI444,
    NI445,
    NI446,
    NI447,
    NI451,
    NI461,
    NI462,
    NI463,
    NI473,
    NI481,
    NI483,
    NI485,
    NI487,
    NI511,
    NI520,
    NI521,
    NI522,
    NI540,
    NI541,
    NI542,
    NI546,
    NI551,
    NI556,
    NI561,
    NI570,
    NI571,
    NI572,
    NI589,
    NI591,
    NI592,
    NI595,
    NI597,
    NI611,
    NI615,
    NI619,
    NI623,
    NI627,
    NI633,
    NI99A,
    NI99B,
    NI99C,
    NI99D,
    NI301,
    NI311,
    NI312,
    NI313,
    NI314,
    NI315,
    NI321,
    NI322,
    NI323,
    NI324,
    NI325,
    NI331,
    NI333,
    NI334,
    NI335,
    NI341,
    NI343,
    NI344,
    NI345,
    NI351,
    NI352,
    NI353,
    NI353,
    NI354,
    NI355,
    NI356,
    NI357,
    NI358,
    NI359,
    NI360,
    NI361,
    NI362,
    NI363,
    NI364,
    NI365,
    NI366,
    NI367,
    NI368,
    NI369,
    NI370,
    NI371,
    NI372,
    NI373,
    NI374,
    NI375,
    NI376,
    NI377,
    NI378,
    NI379,
    NI380
  )

  val custom = TaxCodes.ukTaxCodeSet ++ TaxCodes.euTaxCodeSet

  val exciseTaxCodeSet: Set[TaxCode] = excise.toSet

  def findTaxType(taxCode: TaxCode): String =
    if ukTaxCodeSet.contains(taxCode) then {
      "UK"
    } else if euTaxCodeSet.contains(taxCode) then {
      "EU"
    } else if excise.contains(taxCode) then {
      "Excise"
    } else if vatTaxCodeSet.contains(taxCode) then {
      "VAT"
    } else ""

  def categoryOf(taxCode: TaxCode): String =
    if ukTaxCodeSet.contains(taxCode) then {
      "uk-duty"
    } else if euTaxCodeSet.contains(taxCode) then {
      "eu-duty"
    } else if excise.contains(taxCode) then {
      "excise-duty"
    } else if vatTaxCodeSet.contains(taxCode) then {
      "vat-duty"
    } else "unknown-duty"

  val vatTaxCodes: Seq[TaxCode] = List(B00, B05)

  val vatTaxCodeSet: Set[TaxCode] = vatTaxCodes.toSet

  val all: Seq[TaxCode] = UK ++ EU ++ excise

  def allExcept(taxCodes: Set[TaxCode]): Seq[TaxCode] =
    all.filterNot(taxCodes.contains)

  private[models] val taxCodesStringMap: Map[String, TaxCode] =
    all.map(a => a.value -> a).toMap

  def has(code: String): Boolean =
    TaxCodes.all.exists(_.value === code)

  def find(taxCode: String): Option[TaxCode] =
    taxCodesStringMap.get(taxCode)

  def findUnsafe(taxCode: String): TaxCode =
    taxCodesStringMap.getOrElse(taxCode, UnsupportedTaxCode(taxCode))

}
