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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

trait JourneyGenerators extends JourneyTestData {

  implicit val bigDecimalChoose = new Gen.Choose[BigDecimal] {
    override def choose(min: BigDecimal, max: BigDecimal): Gen[BigDecimal] =
      Gen.choose(1, 10000).map(i => (min + (i * ((max - min) / 10000))).round(min.mc))
  }

  val mrnWithDisplayDeclarationGen: Gen[(MRN, DisplayDeclaration)] =
    IdGen.genMRN
      .flatMap(mrn => displayDeclarationGen.map(d => mrn -> d.withDeclarationId(mrn.value)))

  val displayDeclarationCMAEligibleGen: Gen[DisplayDeclaration]    =
    buildDisplayDeclarationGen(cmaEligible = true)

  val displayDeclarationNotCMAEligibleGen: Gen[DisplayDeclaration] =
    buildDisplayDeclarationGen(cmaEligible = false)

  val displayDeclarationGen: Gen[DisplayDeclaration] =
    Gen.oneOf(
      displayDeclarationCMAEligibleGen,
      displayDeclarationNotCMAEligibleGen
    )

  val exampleDisplayDeclaration: DisplayDeclaration =
    displayDeclarationGen.sample.get

  def buildDisplayDeclarationGen(cmaEligible: Boolean): Gen[DisplayDeclaration] =
    for {
      declarantEORI    <- IdGen.genEori
      consigneeEORI    <- IdGen.genEori
      numberOfTaxCodes <- Gen.choose(1, 5)
      taxCodes         <- Gen.const(TaxCodes.all.take(numberOfTaxCodes))
      paidAmounts      <- Gen.listOfN(numberOfTaxCodes, Gen.choose[BigDecimal](BigDecimal("1.00"), BigDecimal("1000.00")))
    } yield {
      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, cmaEligible) }
      buildDisplayDeclaration(exampleMrnAsString, declarantEORI, Some(consigneeEORI), paidDuties)
    }

}
