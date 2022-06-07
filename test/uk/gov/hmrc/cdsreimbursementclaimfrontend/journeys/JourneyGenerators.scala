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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DEC91Response
import scala.collection.JavaConverters._

trait JourneyGenerators extends JourneyTestData {

  final def listOfExactlyN[A](n: Int, gen: Gen[A]): Gen[List[A]] =
    Gen.sequence((1 to n).map(_ => gen)).map(_.asScala.toList)

  implicit final val bigDecimalChoose = new Gen.Choose[BigDecimal] {
    override def choose(min: BigDecimal, max: BigDecimal): Gen[BigDecimal] =
      Gen.choose(1, 10000).map(i => (min + (i * ((max - min) / 10000))).round(min.mc))
  }

  final lazy val mrnWithDisplayDeclarationGen: Gen[(MRN, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      acc14 <- displayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
               )
    } yield (mrn, acc14)

  final lazy val rfsWithDisplayDeclarationGen: Gen[(ReasonForSecurity, DisplayDeclaration)] =
    for {
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (rfs, acc14)

  final lazy val mrnWithRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  final lazy val mrnWithRfsWithDisplayDeclarationNotGuaranteeEligibleGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationNotGuaranteeEligibleGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  final lazy val mrnWithRfsWithDisplayDeclarationGuaranteeEligibleGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values)
      acc14 <- securitiesDisplayDeclarationGuaranteeEligibleGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  final lazy val mrnWithNonExportRfsWithDisplayDeclarationGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values -- ReasonForSecurity.requiresExportDeclaration)
      acc14 <- securitiesDisplayDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
                   .withReasonForSecurity(rfs)
               )
    } yield (mrn, rfs, acc14)

  final lazy val mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal)])] =
    for {
      (mrn, rfs, decl) <- mrnWithNonExportRfsWithDisplayDeclarationGen
      reclaims         <- validSecurityReclaimsGen(decl)
    } yield (mrn, rfs, decl, reclaims)

  final lazy val exportMrnWithDec91TrueGen: Gen[(MRN, DEC91Response)] =
    for {
      mrn    <- IdGen.genMRN
      status <- Gen.oneOf("21", "22")
      dec91  <- dec91ResponseGen(status)
    } yield (mrn, dec91)

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

  final def taxCodesWithAmountsGen: Gen[Seq[(TaxCode, BigDecimal)]] =
    for {
      numberOfTaxCodes <- Gen.choose(2, 5)
      taxCodes         <- Gen.pick(numberOfTaxCodes, TaxCodes.all)
      amounts          <- listOfExactlyN(numberOfTaxCodes, Gen.choose[BigDecimal](BigDecimal("1.00"), BigDecimal("1000.00")))
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

  final def buildSecuritiesDisplayDeclarationGen(allDutiesGuaranteeEligible: Boolean): Gen[DisplayDeclaration] =
    for {
      reasonForSecurity  <- Gen.oneOf(ReasonForSecurity.values)
      declarantEORI      <- IdGen.genEori
      consigneeEORI      <- IdGen.genEori
      numberOfSecurities <- Gen.choose(2, 5)
      reclaimsDetails    <- listOfExactlyN(
                              numberOfSecurities,
                              Gen.zip(Gen.nonEmptyListOf(Gen.alphaNumChar).map(String.valueOf), taxCodesWithAmountsGen)
                            )
    } yield buildSecuritiesDisplayDeclaration(
      securityReason = reasonForSecurity.acc14Code,
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      reclaimsDetails = reclaimsDetails,
      allDutiesGuaranteeEligible = allDutiesGuaranteeEligible
    )

  final def dec91ResponseGen(status: String): Gen[DEC91Response] =
    Gen.const(DEC91Response(status))

  final def validSecurityReclaimsGen(
    decl: DisplayDeclaration
  ): Gen[Seq[(String, TaxCode, BigDecimal)]] =
    Gen
      .sequence(
        decl.getSecurityDepositIds
          .getOrElse(Seq.empty)
          .halfNonEmpty
          .flatMap(depositId =>
            decl
              .getSecurityDetailsFor(depositId)
              .map(sd =>
                sd.taxDetails.halfNonEmpty.map(td =>
                  Gen
                    .choose(BigDecimal.exact("0.01"), BigDecimal(td.amount))
                    .map(amount => (sd.securityDepositId, TaxCodes.findUnsafe(td.taxType), amount))
                )
              )
              .getOrElse(Seq.empty)
          )
      )
      .map(_.asScala.toSeq)

}
