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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithRefund
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EoriNumbersVerification

/** A collection of generators supporting the tests of RejectedGoodsScheduledJourney. */
object RejectedGoodsScheduledJourneyGenerators extends JourneyGenerators with JourneyTestData {

  val emptyJourney: RejectedGoodsScheduledJourney =
    RejectedGoodsScheduledJourney.empty(exampleEori)

  val journeyWithMrnAndDD: RejectedGoodsScheduledJourney =
    RejectedGoodsScheduledJourney
      .empty(exampleDisplayDeclaration.getDeclarantEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
      .getOrFail

  val amountPaidWithRefundGen: Gen[AmountPaidWithRefund] = for {
    refundAmount <- Gen.choose(BigDecimal("0.01"), BigDecimal("1000.00"))
    random       <- Gen.choose(BigDecimal("0.01"), BigDecimal("100.00"))
    paidAmount    = random + refundAmount
  } yield AmountPaidWithRefund(paidAmount, refundAmount)

  val dutyTypesGen: Gen[Seq[DutyType]] =
    for {
      n   <- Gen.choose(1, DutyTypes.all.size - 1)
      dts <- Gen.pick(n, DutyTypes.all)
    } yield dts.sorted.toSeq

  def taxCodesGen(dutyType: DutyType): Gen[Seq[TaxCode]] =
    for {
      n   <- Gen.choose(1, dutyType.taxCodes.size - 1)
      tcs <- Gen.pick(n, dutyType.taxCodes)
    } yield tcs.sorted.toSeq

  val dutyTypesWithTaxCodesGen: Gen[Seq[(DutyType, Seq[TaxCode])]] = dutyTypesGen.flatMap(dutyTypes =>
    Gen.sequence[Seq[(DutyType, Seq[TaxCode])], (DutyType, Seq[TaxCode])](
      dutyTypes.map(dutyType =>
        for {
          n   <- Gen.choose(1, dutyType.taxCodes.size - 1)
          tcs <- Gen.pick(n, dutyType.taxCodes)
        } yield (dutyType, tcs.sorted.toSeq)
      )
    )
  )

  type TaxCodeWithAmounts = (TaxCode, BigDecimal, BigDecimal)

  def taxCodesWithClaimAmountsGen(dutyType: DutyType): Gen[Seq[TaxCodeWithAmounts]] =
    for {
      n       <- Gen.choose(1, dutyType.taxCodes.size - 1)
      tcs     <- Gen.pick(n, dutyType.taxCodes)
      amounts <- Gen.sequence[Seq[TaxCodeWithAmounts], TaxCodeWithAmounts](
                   tcs.sorted.map(tc =>
                     amountNumberGen
                       .flatMap(pa =>
                         amountNumberInRangeGen(BigDecimal("0.01"), pa)
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
    } yield result.asScala.toSeq

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
    submitBankAccountType: Boolean = true,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[RejectedGoodsScheduledJourney.Features] = None
  ): Gen[RejectedGoodsScheduledJourney] =
    buildJourneyGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      hasConsigneeDetailsInACC14,
      submitConsigneeDetails = submitConsigneeDetails,
      submitContactDetails = submitContactDetails,
      submitContactAddress = submitContactAddress,
      submitBankAccountType = submitBankAccountType,
      submitBankAccountDetails = submitBankAccountDetails,
      generateSubsidyPayments = generateSubsidyPayments,
      features = features
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
    submitBankAccountType: Boolean = true,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[RejectedGoodsScheduledJourney.Features] = None
  ): Gen[Either[String, RejectedGoodsScheduledJourney]] =
    for {
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      reimbursements              <- dutyTypesWithTaxCodesWithClaimAmountsGen
      basisOfClaim                <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      methodOfDisposal            <- Gen.oneOf(MethodOfDisposal.values)
      numberOfSupportingEvidences <- Gen.choose(0, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <-
        Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.rejectedGoodsScheduledDocumentTypes))
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
          reimbursements.flatMap(_._2).map(d => (d._1, d._3, false)),
          if (submitConsigneeDetails) consigneeContact else None,
          declarantContact,
          generateSubsidyPayments = generateSubsidyPayments
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val reimbursementClaims =
        SortedMap(reimbursements: _*).view
          .mapValues(s =>
            SortedMap(s.map { case (taxCode, a1, a2) =>
              (taxCode, Option(AmountPaidWithRefund(a1, a2)))
            }: _*)
          )
          .to(SortedMap)

      val scheduledDocument: UploadedFile =
        buildUploadDocument("schedule").copy(cargo = Some(UploadDocumentType.ScheduleOfMRNs))

      val supportingEvidencesExpanded: Seq[UploadedFile] =
        supportingEvidences.flatMap { case (documentType, size) =>
          (0 until size).map(i => buildUploadDocument(s"$i").copy(cargo = Some(documentType)))
        }.toSeq

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if (submitConsigneeDetails && !hasMatchingEori) {
          if (submitDeclarantDetails)
            Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else
            Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        RejectedGoodsScheduledJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          displayDeclaration = Some(displayDeclaration),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
          contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
          scheduledDocument = Some(scheduledDocument),
          basisOfClaim = Some(basisOfClaim),
          basisOfClaimSpecialCircumstances =
            if (basisOfClaim == BasisOfRejectedGoodsClaim.SpecialCircumstances) Some("special circumstances details")
            else None,
          methodOfDisposal = Some(methodOfDisposal),
          detailsOfRejectedGoods = Some("rejected goods details"),
          reimbursementClaims = Some(reimbursementClaims),
          inspectionDate = Some(exampleInspectionDate),
          inspectionAddress = Some(exampleInspectionAddress),
          selectedDocumentType = None,
          supportingEvidences = supportingEvidencesExpanded,
          bankAccountDetails = if (submitBankAccountDetails) Some(exampleBankAccountDetails) else None,
          bankAccountType = if (submitBankAccountType) Some(bankAccountType) else None,
          checkYourAnswersChangeMode = true
        )

      RejectedGoodsScheduledJourney.tryBuildFrom(answers, features)
    }

}
