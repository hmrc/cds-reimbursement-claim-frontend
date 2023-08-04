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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes

import scala.jdk.CollectionConverters._

trait JourneyGenerators extends JourneyTestData with BigDecimalGen {

  final def listOfExactlyN[A](n: Int, gen: Gen[A]): Gen[List[A]] =
    Gen.sequence((1 to n).map(_ => gen)).map(_.asScala.toList)

  final lazy val mrnWithDisplayDeclarationGen: Gen[(MRN, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      acc14 <- displayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
               )
    } yield (mrn, acc14)

  final lazy val mrnWithDisplayDeclarationSubsidyOnlyGen: Gen[(MRN, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      acc14 <- displayDeclarationSubsidyOnly.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
               )
    } yield (mrn, acc14)

  final val displayDeclarationCMAEligibleGen: Gen[DisplayDeclaration] =
    buildDisplayDeclarationGen(cmaEligible = true)

  final val displayDeclarationNotCMAEligibleGen: Gen[DisplayDeclaration] =
    buildDisplayDeclarationGen(cmaEligible = false)

  final val displayDeclarationSubsidyOnly: Gen[DisplayDeclaration] =
    buildDisplayDeclarationGen(cmaEligible = false, subsidyPayments = GenerateSubsidyPayments.All)

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

  final def exampleDisplayDeclarationWithNIExciseCodes: DisplayDeclaration =
    (for {
      declarantEORI <- IdGen.genEori
      consigneeEORI <- IdGen.genEori
      paidAmounts   <- taxCodesWithAmountsGen(TaxCodes.excise)
    } yield buildDisplayDeclaration(
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      dutyDetails = (paidAmounts :+ ((TaxCode.A00, BigDecimal("1.01")))).map { case (t, a) => (t, a, false) }
    )).sample.get

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
    } yield taxCodes.zip(amounts).toSeq

  final def taxCodesWithAmountsGen(taxCodes: Seq[TaxCode]): Gen[Seq[(TaxCode, BigDecimal)]] =
    for {
      numberOfTaxCodes <- Gen.choose(2, Math.min(5, taxCodes.size))
      taxCodes         <- Gen.pick(numberOfTaxCodes, taxCodes)
      amounts          <-
        listOfExactlyN(
          numberOfTaxCodes,
          amountNumberGen
        )
    } yield taxCodes.zip(amounts).toSeq

  final def buildDisplayDeclarationGen(
    cmaEligible: Boolean,
    subsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None
  ): Gen[DisplayDeclaration] =
    for {
      declarantEORI <- IdGen.genEori
      consigneeEORI <- IdGen.genEori
      paidAmounts   <- taxCodesWithAmountsGen
    } yield buildDisplayDeclaration(
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      dutyDetails = paidAmounts.map { case (t, a) => (t, a, cmaEligible) },
      generateSubsidyPayments = subsidyPayments
    )

  final lazy val depositIdGen: Gen[String] =
    listOfExactlyN(6, Gen.oneOf("ABCDEFGHIJKLMNOPRSTUWXYZ0123456789".toCharArray().toIndexedSeq)).map(l =>
      String.valueOf(l.toArray)
    )

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
