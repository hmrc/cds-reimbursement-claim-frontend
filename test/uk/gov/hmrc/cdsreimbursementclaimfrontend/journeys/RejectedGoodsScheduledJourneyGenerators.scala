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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

import scala.collection.JavaConverters._

/** A collection of generators supporting the tests of RejectedGoodsScheduledJourney. */
object RejectedGoodsScheduledJourneyGenerators extends JourneyGenerators with RejectedGoodsScheduledJourneyTestData {

  val dutyTypesGen: Gen[Seq[DutyType]] =
    for {
      n   <- Gen.choose(1, DutyTypes.all.size - 1)
      dts <- Gen.pick(n, DutyTypes.all)
    } yield dts

  def taxCodesGen(dutyType: DutyType): Gen[Seq[TaxCode]] =
    for {
      n   <- Gen.choose(1, dutyType.taxCodes.size - 1)
      tcs <- Gen.pick(n, dutyType.taxCodes)
    } yield tcs

  val dutyTypesWithTaxCodesGen: Gen[Seq[(DutyType, Seq[TaxCode])]] = dutyTypesGen.flatMap(dutyTypes =>
    Gen.sequence[Seq[(DutyType, Seq[TaxCode])], (DutyType, Seq[TaxCode])](
      dutyTypes.map(dutyType =>
        for {
          n   <- Gen.choose(1, dutyType.taxCodes.size - 1)
          tcs <- Gen.pick(n, dutyType.taxCodes)
        } yield (dutyType, tcs)
      )
    )
  )

  type TaxCodeWithAmounts = (TaxCode, BigDecimal, BigDecimal)

  def taxCodesWithClaimAmountsGen(dutyType: DutyType): Gen[Seq[TaxCodeWithAmounts]] =
    for {
      n       <- Gen.choose(1, dutyType.taxCodes.size - 1)
      tcs     <- Gen.pick(n, dutyType.taxCodes)
      amounts <- Gen.sequence[Seq[TaxCodeWithAmounts], TaxCodeWithAmounts](
                   tcs.map(tc =>
                     Gen
                       .choose[BigDecimal](BigDecimal("0.01"), BigDecimal("1000.00"))
                       .flatMap(pa =>
                         Gen
                           .choose[BigDecimal](BigDecimal("0.01"), pa)
                           .map(ra => (tc, ra, pa))
                       )
                   )
                 )
    } yield amounts

  val dutyTypesWithTaxCodesWithClaimAmountsGen: Gen[Seq[(DutyType, Seq[TaxCodeWithAmounts])]] =
    for {
      dutyTypes <- dutyTypesGen
      result    <-
        Gen.sequence(dutyTypes.map(dutyType => taxCodesWithClaimAmountsGen(dutyType).map(tcs => dutyType -> tcs)))
    } yield result.asScala

  val completeJourneyWithMatchingUserEoriGen: Gen[RejectedGoodsScheduledJourney] =
    Gen.oneOf(
      buildCompleteJourneyGen(
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteJourneyGen(
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteJourneyGen(
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeJourneyWithNonNatchingUserEoriGen: Gen[RejectedGoodsScheduledJourney] =
    buildCompleteJourneyGen(
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyGen: Gen[RejectedGoodsScheduledJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriGen,
      completeJourneyWithNonNatchingUserEoriGen
    )

  val completeJourneyGenWithoutSpecialCircumstances: Gen[RejectedGoodsScheduledJourney] = for {
    journey      <- completeJourneyGen
    basisOfClaim <- Gen.oneOf(BasisOfRejectedGoodsClaim.values - BasisOfRejectedGoodsClaim.SpecialCircumstances)
  } yield journey.submitBasisOfClaim(basisOfClaim)

  val completeJourneyGenWithSpecialCircumstances: Gen[RejectedGoodsScheduledJourney] = for {
    journey                          <- completeJourneyGen
    basisOfClaimSpecialCircumstances <- genStringWithMaxSizeOfN(500)
  } yield journey
    .submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
    .submitBasisOfClaimSpecialCircumstancesDetails(basisOfClaimSpecialCircumstances)
    .fold(error => throw new Exception(error), identity)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def buildCompleteJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true
  ): Gen[RejectedGoodsScheduledJourney] =
    buildJourneyGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      hasConsigneeDetailsInACC14,
      submitConsigneeDetails = submitConsigneeDetails,
      submitContactDetails = submitContactDetails,
      submitContactAddress = submitContactAddress,
      submitBankAccountType = submitBankAccountType,
      submitBankAccountDetails = submitBankAccountDetails
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannnot build complete RejectedGoodsScheduledJourney because of $error, fix the test data generator."
          ),
        identity
      )
    )

  def buildJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true
  ): Gen[Either[String, RejectedGoodsScheduledJourney]] =
    for {
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      reimbursementClaims         <- dutyTypesWithTaxCodesWithClaimAmountsGen
      basisOfClaim                <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      methodOfDisposal            <- Gen.oneOf(MethodOfDisposal.values)
      numberOfSupportingEvidences <- Gen.choose(0, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <- Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.rejectedGoodsScheduledTypes))
      supportingEvidences         <-
        Gen
          .sequence[Seq[(UploadDocumentType, Int)], (UploadDocumentType, Int)](
            documentTypes.map(dt => Gen.choose(0, numberOfSupportingEvidences).map(n => (dt, n)))
          )
          .map(_.toMap)
      bankAccountType             <- Gen.oneOf(BankAccountType.values)
      consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
    } yield {

      val displayDeclaration: DisplayDeclaration =
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
          reimbursementClaims.flatMap(_._2).map(d => (d._1, d._3, false)),
          if (submitConsigneeDetails) consigneeContact else None,
          declarantContact
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      tryBuildRejectedGoodsScheduledJourney(
        userEoriNumber,
        mrn,
        displayDeclaration,
        basisOfClaim,
        "rejected goods details",
        "special circumstances details",
        exampleInspectionDate,
        exampleInspectionAddress,
        methodOfDisposal,
        reimbursementClaims,
        supportingEvidences,
        declarantEoriNumber = if (submitDeclarantDetails && !hasMatchingEori) Some(declarantEORI) else None,
        consigneeEoriNumber = if (submitConsigneeDetails && !hasMatchingEori) Some(consigneeEORI) else None,
        contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
        contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
        bankAccountDetails = if (submitBankAccountDetails) Some(exampleBankAccountDetails) else None,
        bankAccountType = if (submitBankAccountType) Some(bankAccountType) else None
      )
    }

}
