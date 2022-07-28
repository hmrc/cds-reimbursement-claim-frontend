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
import play.api.Logger
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.SecurityDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import scala.collection.JavaConverters._

trait JourneyGenerators extends JourneyTestData {

  final def listOfExactlyN[A](n: Int, gen: Gen[A]): Gen[List[A]] =
    Gen.sequence((1 to n).map(_ => gen)).map(_.asScala.toList)

  implicit final val bigDecimalChoose = new Gen.Choose[BigDecimal] {
    override def choose(min: BigDecimal, max: BigDecimal): Gen[BigDecimal] =
      Gen.choose(1, 10000).map(i => (min + (i * ((max - min) / 10000))).round(min.mc))
  }

  final lazy val amountNumberGen: Gen[BigDecimal] =
    amountNumberInRangeGen(BigDecimal("1.00"), BigDecimal("1000.00"))

  final def amountNumberInRangeGen(minIncl: BigDecimal, maxIncl: BigDecimal): Gen[BigDecimal] =
    Gen
      .choose[BigDecimal](minIncl, maxIncl)
      .map(bd => BigDecimal(bd.*(100).toInt)./(100))

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

  final lazy val mrnWithNonExportRfsWithDisplayDeclarationNotGuaranteeEligibleGen: Gen[(MRN, ReasonForSecurity, DisplayDeclaration)] =
    for {
      mrn   <- IdGen.genMRN
      rfs   <- Gen.oneOf(ReasonForSecurity.values -- ReasonForSecurity.requiresExportDeclaration)
      acc14 <- securitiesDisplayDeclarationNotGuaranteeEligibleGen.map(
        _.withDeclarationId(mrn.value)
          .withDeclarantEori(exampleEori)
          .withReasonForSecurity(rfs)
      )
    } yield (mrn, rfs, acc14)

  final lazy val mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal)])] =
    for {
      (mrn, rfs, decl) <- mrnWithRfsWithDisplayDeclarationGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    } yield (mrn, rfs, decl, reclaims)

  final lazy val mrnIncludingExportRfsWithDisplayDeclarationWithReclaimsGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal)])] =
    for {
      (mrn, rfs, decl) <- mrnWithRfsWithDisplayDeclarationGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    } yield (mrn, rfs, decl, reclaims)

  final lazy val mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsNotGuaranteeEligibleGen
    : Gen[(MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal)])] =
    for {
      (mrn, rfs, decl) <- mrnWithNonExportRfsWithDisplayDeclarationNotGuaranteeEligibleGen
      reclaims         <- validSecurityReclaimsGen(decl)
    } yield (mrn, rfs, decl, reclaims)

  final lazy val exportMrnTrueGen: Gen[MRN] =
    for {
      mrn <- IdGen.genMRN
    } yield mrn

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

  final def validSecurityReclaimsGen(decl: DisplayDeclaration): Gen[Seq[(String, TaxCode, BigDecimal)]] =
    validSecurityReclaimsGenInternal(
      decl,
      sd =>
        sd.taxDetails.halfNonEmpty.map(td =>
          Gen
            .choose(BigDecimal.exact("0.01"), BigDecimal(td.amount))
            .map(reclaimAmount => reclaimAmount)
            .map(amount => (sd.securityDepositId, TaxCodes.findUnsafe(td.taxType), amount))
        )
    )

  final def validSecurityReclaimsFullAmountGen(decl: DisplayDeclaration): Gen[Seq[(String, TaxCode, BigDecimal)]] =
    validSecurityReclaimsGenInternal(
      decl,
      sd =>
        sd.taxDetails.map(td =>
          Gen
            .map(reclaimAmount => BigDecimal(td.amount))
            .map(amount => (sd.securityDepositId, TaxCodes.findUnsafe(td.taxType), amount))
        )
    )
  val logger: Logger                                                                                              = Logger(this.getClass)

  private final def validSecurityReclaimsGenInternal(
    decl: DisplayDeclaration,
    mapSecurityDeposit: SecurityDetails => Seq[Gen[(String, TaxCode, BigDecimal)]]
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
                    .choose(BigDecimal.exact("0.01"), td.getAmount)
                    .map(amount => (sd.securityDepositId, td.getTaxCode, amount))
                )
              )
              .getOrElse(Seq.empty)
          )
      )
      .map(_.asScala.toSeq)

}
