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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.claims

import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.EndUseRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes

import scala.jdk.CollectionConverters.*

/** Common functions and helpers supporting creation of the test claims. */
trait ClaimGenerators extends ClaimTestData with BigDecimalGen {

  final val ZERO = BigDecimal("0.00")

  final def listOfExactlyN[A](n: Int, gen: Gen[A]): Gen[List[A]] =
    Gen.sequence((1 to n).map(_ => gen)).map(_.asScala.toList)

  final lazy val mrnWithImportDeclarationGen: Gen[(MRN, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      acc14 <- importDeclarationGen.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
               )
    yield (mrn, acc14)

  final lazy val mrnWithImportDeclarationSubsidyOnlyGen: Gen[(MRN, ImportDeclaration)] =
    for
      mrn   <- IdGen.genMRN
      acc14 <- importDeclarationSubsidyOnly.map(
                 _.withDeclarationId(mrn.value)
                   .withDeclarantEori(exampleEori)
               )
    yield (mrn, acc14)

  final val importDeclarationCMAEligibleGen: Gen[ImportDeclaration] =
    buildImportDeclarationGen(cmaEligible = true)

  final val importDeclarationNotCMAEligibleGen: Gen[ImportDeclaration] =
    buildImportDeclarationGen(cmaEligible = false)

  final val importDeclarationSubsidyOnly: Gen[ImportDeclaration] =
    buildImportDeclarationGen(cmaEligible = false, subsidyPayments = GenerateSubsidyPayments.All)

  final lazy val importDeclarationGen: Gen[ImportDeclaration] =
    Gen.oneOf(
      importDeclarationCMAEligibleGen,
      importDeclarationNotCMAEligibleGen
    )

  final val securitiesImportDeclarationGuaranteeEligibleGen: Gen[ImportDeclaration] =
    buildSecuritiesImportDeclarationGen(allDutiesGuaranteeEligible = true)

  final val securitiesImportDeclarationNotGuaranteeEligibleGen: Gen[ImportDeclaration] =
    buildSecuritiesImportDeclarationGen(allDutiesGuaranteeEligible = false)

  final val securitiesSingleImportDeclarationGuaranteeEligibleGen: Gen[ImportDeclaration] =
    buildSingleSecurityImportDeclarationGen(allDutiesGuaranteeEligible = true)

  final val securitiesSingleImportDeclarationNotGuaranteeEligibleGen: Gen[ImportDeclaration] =
    buildSingleSecurityImportDeclarationGen(allDutiesGuaranteeEligible = false)

  final lazy val securitiesImportDeclarationGen: Gen[ImportDeclaration] =
    Gen.oneOf(
      securitiesImportDeclarationGuaranteeEligibleGen,
      securitiesImportDeclarationNotGuaranteeEligibleGen
    )

  final lazy val securitiesSingleImportDeclarationGen: Gen[ImportDeclaration] =
    Gen.oneOf(
      securitiesSingleImportDeclarationGuaranteeEligibleGen,
      securitiesSingleImportDeclarationNotGuaranteeEligibleGen
    )

  final lazy val securitiesImportDeclarationWithoutIPROrEndUseReliefGen =
    securitiesImportDeclarationGen
      .suchThat(
        _.getReasonForSecurity.exists(rfs =>
          rfs != ReasonForSecurity.InwardProcessingRelief
            && rfs != ReasonForSecurity.EndUseRelief
        )
      )

  final val exampleImportDeclaration: ImportDeclaration =
    importDeclarationGen.sample.get

  final def exampleImportDeclarationWithNIExciseCodes: ImportDeclaration =
    (for
      declarantEORI <- IdGen.genEori
      consigneeEORI <- IdGen.genEori
      paidAmounts   <- taxCodesWithAmountsGen(TaxCodes.excise)
    yield buildImportDeclaration(
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      dutyDetails = (paidAmounts :+ ((TaxCode.A00, BigDecimal("1.01")))).map { case (t, a) => (t, a, false) }
    )).sample.get

  final def exampleImportDeclarationWithSomeUnsupportedCode: ImportDeclaration =
    (for
      declarantEORI <- IdGen.genEori
      consigneeEORI <- IdGen.genEori
      paidAmounts   <- taxCodesWithAmountsGen(Seq(TaxCode.A00, TaxCode.UnsupportedTaxCode("foo")))
    yield buildImportDeclaration(
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      dutyDetails = paidAmounts.map { case (t, a) => (t, a, false) }
    )).sample.get

  final def exampleImportDeclarationWithOnlyUnsupportedCodes: ImportDeclaration =
    (for
      declarantEORI <- IdGen.genEori
      consigneeEORI <- IdGen.genEori
      paidAmounts   <- taxCodesWithAmountsGen(Seq(TaxCode.UnsupportedTaxCode("foo"), TaxCode.UnsupportedTaxCode("bar")))
    yield buildImportDeclaration(
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      dutyDetails = paidAmounts.map { case (t, a) => (t, a, false) }
    )).sample.get

  final val exampleSecuritiesImportDeclaration: ImportDeclaration =
    securitiesImportDeclarationGen.sample.get

  final def taxCodesWithAmountsGen: Gen[Seq[(TaxCode, BigDecimal)]] =
    for
      numberOfTaxCodes <- Gen.choose(2, 5)
      taxCodes         <- Gen.pick(numberOfTaxCodes, TaxCodes.all).map(_.distinct)
      amounts          <-
        listOfExactlyN(
          numberOfTaxCodes,
          amountNumberGen
        )
    yield taxCodes.distinct.zip(amounts).toSeq

  final def taxCodesWithAmountsGen(numberOfDutyTypes: Option[Int] = None): Gen[Seq[(TaxCode, BigDecimal)]] =
    for
      numberOfTaxCodes <- numberOfDutyTypes.map(Gen.const).getOrElse(Gen.choose(2, 5))
      taxCodes         <- Gen.pick(numberOfTaxCodes, TaxCodes.all).map(_.distinct)
      amounts          <-
        listOfExactlyN(
          numberOfTaxCodes,
          amountNumberGen
        )
    yield taxCodes.distinct.zip(amounts).toSeq

  final def taxCodesWithAmountsGen(taxCodes: Seq[TaxCode]): Gen[Seq[(TaxCode, BigDecimal)]] =
    for
      numberOfTaxCodes <- Gen.choose(2, Math.min(5, taxCodes.size))
      taxCodes         <- Gen.pick(numberOfTaxCodes, taxCodes)
      amounts          <-
        listOfExactlyN(
          numberOfTaxCodes,
          amountNumberGen
        )
    yield taxCodes.distinct.zip(amounts).toSeq

  final def buildImportDeclarationGen(
    cmaEligible: Boolean,
    subsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None
  ): Gen[ImportDeclaration] =
    for
      declarantEORI <- IdGen.genEori
      consigneeEORI <- IdGen.genEori
      paidAmounts   <- taxCodesWithAmountsGen
    yield buildImportDeclaration(
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      dutyDetails = paidAmounts.map { case (t, a) => (t, a, cmaEligible) },
      generateSubsidyPayments = subsidyPayments
    )

  final lazy val depositIdGen: Gen[String] =
    listOfExactlyN(6, Gen.oneOf("ABCDEFGHIJKLMNOPRSTUWXYZ0123456789".toCharArray().toIndexedSeq)).map(l =>
      String.valueOf(l.toArray)
    )

  final def buildSecuritiesImportDeclarationGen(allDutiesGuaranteeEligible: Boolean): Gen[ImportDeclaration] =
    for
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
    yield buildSecuritiesImportDeclaration(
      securityReason = reasonForSecurity.acc14Code,
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      depositDetails = reasonForSecurity match {
        case EndUseRelief =>
          reclaimsDetails.map { rcd =>
            (rcd._1, rcd._2.filterNot(td => td._1.isVAT))
          }
        case _            => reclaimsDetails
      },
      allDutiesGuaranteeEligible = allDutiesGuaranteeEligible,
      declarantContact = Some(declarantContact)
    )

  final def buildSingleSecurityImportDeclarationGen(allDutiesGuaranteeEligible: Boolean): Gen[ImportDeclaration] =
    for
      reasonForSecurity  <- Gen.oneOf(ReasonForSecurity.values)
      declarantEORI      <- IdGen.genEori
      consigneeEORI      <- IdGen.genEori
      numberOfSecurities <- Gen.const(1)
      reclaimsDetails    <- Gen.zip(depositIdGen, taxCodesWithAmountsGen).map(Seq(_))
      declarantContact   <- Acc14Gen.genContactDetails
    yield buildSecuritiesImportDeclaration(
      securityReason = reasonForSecurity.acc14Code,
      declarantEORI = declarantEORI,
      consigneeEORI = Some(consigneeEORI),
      depositDetails = reasonForSecurity match {
        case EndUseRelief =>
          reclaimsDetails.map { rcd =>
            (rcd._1, rcd._2.filterNot(td => td._1.isVAT))
          }
        case _            => reclaimsDetails
      },
      allDutiesGuaranteeEligible = allDutiesGuaranteeEligible,
      declarantContact = Some(declarantContact)
    )

}
