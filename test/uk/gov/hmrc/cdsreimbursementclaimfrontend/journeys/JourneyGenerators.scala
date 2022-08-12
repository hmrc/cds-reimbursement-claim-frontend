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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import scala.collection.JavaConverters._

trait JourneyGenerators extends JourneyTestData {

  final def listOfExactlyN[A](n: Int, gen: Gen[A]): Gen[List[A]] =
    Gen.sequence((1 to n).map(_ => gen)).map(_.asScala.toList)

  implicit final val bigDecimalChoose = new Gen.Choose[BigDecimal] {
    override def choose(min: BigDecimal, max: BigDecimal): Gen[BigDecimal] =
      Gen
        .choose(1, 10000)
        .map(i => (min + (i * ((max - min) / 10000))))
        .map(bd => BigDecimal(bd.*(100).toInt)./(100))
  }

  final lazy val amountNumberGen: Gen[BigDecimal] =
    amountNumberInRangeGen(BigDecimal("1.00"), BigDecimal("1000.00"))

  final def amountNumberInRangeGen(minIncl: BigDecimal, maxIncl: BigDecimal): Gen[BigDecimal] =
    Gen.choose[BigDecimal](minIncl, maxIncl)

  final lazy val mrnWithDisplayDeclarationGen: Gen[(MRN, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      acc14 <- displayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
               )
    } yield (mrn, acc14)

  final val displayDeclarationCMAEligibleGen: Gen[DisplayDeclaration] =
    buildDisplayDeclarationGen(cmaEligible = true)

  final val displayDeclarationNotCMAEligibleGen: Gen[DisplayDeclaration] =
    buildDisplayDeclarationGen(cmaEligible = false)

  final lazy val displayDeclarationGen: Gen[DisplayDeclaration] =
    Gen.oneOf(
      displayDeclarationCMAEligibleGen,
      displayDeclarationNotCMAEligibleGen
    )

  final val securitiesDisplayDeclarationGuaranteeEligibleGen: Gen[DisplayDeclaration] =
    buildSecuritiesDisplayDeclarationGen(allDutiesGuaranteeEligible = true)

  final val securitiesDisplayDeclarationNotGuaranteeEligibleGen: Gen[DisplayDeclaration] =
    buildSecuritiesDisplayDeclarationGen(allDutiesGuaranteeEligible = false)

  final lazy val securitiesDisplayDeclarationGen: Gen[DisplayDeclaration] =
    Gen.oneOf(
      securitiesDisplayDeclarationGuaranteeEligibleGen,
      securitiesDisplayDeclarationNotGuaranteeEligibleGen
    )

  final val exampleDisplayDeclaration: DisplayDeclaration =
    displayDeclarationGen.sample.get

  final val exampleSecuritiesDisplayDeclaration: DisplayDeclaration =
    securitiesDisplayDeclarationGen.sample.get

  final def taxCodesWithAmountsGen: Gen[Seq[(TaxCode, BigDecimal)]] =
    for {
      numberOfTaxCodes <- Gen.choose(2, 5)
      taxCodes         <- Gen.pick(numberOfTaxCodes, TaxCodes.all)
      amounts          <-
        listOfExactlyN(
          numberOfTaxCodes,
          amountNumberGen
        )
    } yield taxCodes.zip(amounts)

  final def buildDisplayDeclarationGen(cmaEligible: Boolean): Gen[DisplayDeclaration] =
    for {
      declarantEORI <- IdGen.genEori
      consigneeEORI <- IdGen.genEori
      paidAmounts   <- taxCodesWithAmountsGen
    } yield buildDisplayDeclaration(
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      dutyDetails = paidAmounts.map { case (t, a) => (t, a, cmaEligible) }
    )

  final lazy val depositIdGen: Gen[String] =
    listOfExactlyN(6, Gen.oneOf("ABCDEFGHIJKLMNOPRSTUWXYZ0123456789".toCharArray())).map(l => String.valueOf(l.toArray))

  final def buildSecuritiesDisplayDeclarationGen(allDutiesGuaranteeEligible: Boolean): Gen[DisplayDeclaration] =
    for {
      reasonForSecurity  <- Gen.oneOf(ReasonForSecurity.values)
      declarantEORI      <- IdGen.genEori
      consigneeEORI      <- IdGen.genEori
      numberOfSecurities <- Gen.choose(2, 5)
      reclaimsDetails    <-
        listOfExactlyN(
          numberOfSecurities,
          Gen.zip(depositIdGen, taxCodesWithAmountsGen)
        )
      declarantContact   <- Acc14Gen.genContactDetails
    } yield buildSecuritiesDisplayDeclaration(
      securityReason = reasonForSecurity.acc14Code,
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      reclaimsDetails = reclaimsDetails,
      allDutiesGuaranteeEligible = allDutiesGuaranteeEligible,
      declarantContact = Some(declarantContact)
    )

}
