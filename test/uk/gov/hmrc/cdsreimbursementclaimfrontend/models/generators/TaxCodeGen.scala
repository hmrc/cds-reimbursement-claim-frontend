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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.magnolia.Typeclass
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

object TaxCodeGen {

  lazy val genTaxCode: Gen[TaxCode] =
    Gen.oneOf(DutyTypes.all.map(_.taxCodes).distinct.reduce(_ ++ _))

  lazy val genTaxCodes: Gen[List[TaxCode]] =
    Gen.listOf(genTaxCode).map(_.distinct)

  implicit lazy val arbitraryTaxCodeGen: Typeclass[TaxCode] =
    Arbitrary(genTaxCode)

  implicit lazy val arbitraryTaxCodes: Typeclass[List[TaxCode]] =
    Arbitrary(genTaxCodes)
}
