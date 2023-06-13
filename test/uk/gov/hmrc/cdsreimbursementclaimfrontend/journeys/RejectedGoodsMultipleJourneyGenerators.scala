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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OrderedMap

import scala.jdk.CollectionConverters._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EoriNumbersVerification

/** A collection of generators supporting the tests of RejectedGoodsMultipleJourney. */
object RejectedGoodsMultipleJourneyGenerators extends JourneyGenerators with JourneyTestData {

  val journeyWithMrnAndDD: RejectedGoodsMultipleJourney =
    RejectedGoodsMultipleJourney
      .empty(exampleDisplayDeclaration.getDeclarantEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
      .getOrFail

  final val emptyJourney: RejectedGoodsMultipleJourney =
    RejectedGoodsMultipleJourney.empty(exampleEori)

  def incompleteJourneyWithMrnsGen(n: Int): Gen[(RejectedGoodsMultipleJourney, Seq[MRN])] = {
    def submitData(journey: RejectedGoodsMultipleJourney)(data: ((MRN, DisplayDeclaration), Int)) =
      journey.submitMovementReferenceNumberAndDeclaration(data._2, data._1._1, data._1._2)

    listOfExactlyN(n, mrnWithDisplayDeclarationGen).map { data =>
      val dataWithIndex: List[((MRN, DisplayDeclaration), Int)] = data.zipWithIndex
      (
        emptyJourney
          .flatMapEach(dataWithIndex, submitData)
          .getOrFail,
        data.map(_._1)
      )
    }
  }

  private def mrnWithSelectedTaxCodesGen(journey: RejectedGoodsMultipleJourney): Seq[Gen[(MRN, Seq[TaxCode])]] =
    journey.answers.movementReferenceNumbers.get.map { mrn =>
      val availableTaxCodes = journey.getAvailableDuties(mrn).map(_._1)
      Gen
        .choose(1, availableTaxCodes.size)
        .map(availableTaxCodes.take)
        .map(seq => (mrn, seq))
    }

  def incompleteJourneyWithSelectedDutiesGen(n: Int): Gen[(RejectedGoodsMultipleJourney, Seq[MRN])] = {
    def submitData(journey: RejectedGoodsMultipleJourney)(data: (MRN, Seq[TaxCode])) =
      journey.selectAndReplaceTaxCodeSetForReimbursement(data._1, data._2)

    incompleteJourneyWithMrnsGen(n).flatMap { case (journey, _) =>
      val gen = mrnWithSelectedTaxCodesGen(journey)
      Gen
        .sequence[Seq[(MRN, Seq[TaxCode])], (MRN, Seq[TaxCode])](gen)
        .map { mrnsWithTaxCodesSelection =>
          val modifiedJourney = journey
            .flatMapEach(mrnsWithTaxCodesSelection, submitData)
            .getOrFail
          (
            modifiedJourney,
            modifiedJourney.answers.movementReferenceNumbers.get
          )
        }
    }
  }

  def incompleteJourneyWithCompleteClaimsGen(n: Int): Gen[(RejectedGoodsMultipleJourney, Seq[MRN])] = {
    def submitData(journey: RejectedGoodsMultipleJourney)(data: (MRN, TaxCode, BigDecimal)) =
      journey.submitAmountForReimbursement(data._1, data._2, data._3)

    incompleteJourneyWithSelectedDutiesGen(n).map { case (journey, mrns) =>
      val data: Seq[(MRN, TaxCode, BigDecimal)] = mrns.flatMap { mrn =>
        journey.getSelectedDuties(mrn).get.map { taxCode =>
          (mrn, taxCode, BigDecimal(formatAmount(journey.getAmountPaidFor(mrn, taxCode).get / 2)))
        }
      }
      (journey.flatMapEach(data, submitData).getOrFail, mrns)
    }
  }

  val completeJourneyWithMatchingUserEoriAndCMAEligibleGen: Gen[RejectedGoodsMultipleJourney] =
    Gen.oneOf(
      buildCompleteJourneyGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteJourneyGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteJourneyGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeJourneyWithMatchingUserEoriAndNotCMAEligibleGen: Gen[RejectedGoodsMultipleJourney] =
    Gen.oneOf(
      buildCompleteJourneyGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteJourneyGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteJourneyGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeJourneyWithNonNatchingUserEoriAndCMAEligibleGen: Gen[RejectedGoodsMultipleJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = true,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyWithNonNatchingUserEoriAndNotCMAEligibleGen: Gen[RejectedGoodsMultipleJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = false,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyCMAEligibleGen: Gen[RejectedGoodsMultipleJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriAndCMAEligibleGen,
      completeJourneyWithNonNatchingUserEoriAndCMAEligibleGen
    )

  val completeJourneyNotCMAEligibleGen: Gen[RejectedGoodsMultipleJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriAndNotCMAEligibleGen,
      completeJourneyWithNonNatchingUserEoriAndNotCMAEligibleGen
    )

  val completeJourneyGen: Gen[RejectedGoodsMultipleJourney] =
    Gen.oneOf(
      completeJourneyCMAEligibleGen,
      completeJourneyNotCMAEligibleGen
    )

  val completeJourneyWithOnlySubsidiesGen: Gen[RejectedGoodsMultipleJourney] =
    buildCompleteJourneyGen(
      generateSubsidyPayments = GenerateSubsidyPayments.All,
      acc14ConsigneeMatchesUserEori = true,
      acc14DeclarantMatchesUserEori = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false,
      features = Some(RejectedGoodsMultipleJourney.Features(false, true))
    )

  val completeJourneyWithSomeSubsidiesGen: Gen[RejectedGoodsMultipleJourney] =
    buildCompleteJourneyGen(
      generateSubsidyPayments = GenerateSubsidyPayments.Some,
      acc14ConsigneeMatchesUserEori = true,
      acc14DeclarantMatchesUserEori = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false,
      features = Some(RejectedGoodsMultipleJourney.Features(false, true))
    )

  val completeJourneyGenWithoutSpecialCircumstances: Gen[RejectedGoodsMultipleJourney] = for {
    journey      <- completeJourneyGen
    basisOfClaim <- Gen.oneOf(BasisOfRejectedGoodsClaim.values - BasisOfRejectedGoodsClaim.SpecialCircumstances)
  } yield journey.submitBasisOfClaim(basisOfClaim)

  val completeJourneyGenWithSpecialCircumstances: Gen[RejectedGoodsMultipleJourney] = for {
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
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[RejectedGoodsMultipleJourney.Features] = None
  ): Gen[RejectedGoodsMultipleJourney] =
    buildJourneyGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      allDutiesCmaEligible,
      hasConsigneeDetailsInACC14,
      submitConsigneeDetails = submitConsigneeDetails,
      submitContactDetails = submitContactDetails,
      submitContactAddress = submitContactAddress,
      submitBankAccountDetails = submitBankAccountDetails,
      submitBankAccountType = submitBankAccountType,
      minNumberOfMRNs = minNumberOfMRNs,
      maxNumberOfMRNs = maxNumberOfMRNs,
      maxSize = maxSize,
      generateSubsidyPayments = generateSubsidyPayments,
      features = features
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannnot build complete RejectedGoodsMultipleJourney because of $error, fix the test data generator."
          ),
        identity
      )
    )

  type TaxCodesAndAmounts = (Seq[TaxCode], Seq[TaxCode], List[BigDecimal], Seq[BigDecimal])

  def taxCodesAndAmountsGen(maxSize: Int): Gen[TaxCodesAndAmounts] = for {
    numberOfTaxCodes         <- Gen.choose(1, maxSize)
    numberOfSelectedTaxCodes <- Gen.choose(1, numberOfTaxCodes)
    taxCodes                 <- Gen.pick(numberOfTaxCodes, TaxCodes.all)
    paidAmounts              <- Gen.listOfN(numberOfTaxCodes, amountNumberGen)
    reimbursementAmounts     <-
      Gen
        .sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.take(numberOfSelectedTaxCodes).map(a => Gen.choose(BigDecimal.exact("0.01"), a))
        )
  } yield (taxCodes.toSeq, taxCodes.take(numberOfSelectedTaxCodes).toSeq, paidAmounts, reimbursementAmounts)

  def buildJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[RejectedGoodsMultipleJourney.Features] = None
  ): Gen[Either[String, RejectedGoodsMultipleJourney]] =
    for {
      userEoriNumber      <- IdGen.genEori
      numberOfMRNs        <- Gen.choose(minNumberOfMRNs, Math.max(minNumberOfMRNs, maxNumberOfMRNs))
      mrns                <- Gen.listOfN(numberOfMRNs, IdGen.genMRN)
      declarantEORI       <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI       <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      taxCodesWithAmounts <- Gen.sequence(mrns.map(_ => taxCodesAndAmountsGen(maxSize))).map(_.asScala)
      basisOfClaim        <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      methodOfDisposal    <- Gen.oneOf(MethodOfDisposal.values)

      numberOfSupportingEvidences <- Gen.choose(0, maxSize)
      numberOfDocumentTypes       <- Gen.choose(1, maxSize - 1)
      documentTypes               <-
        Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.rejectedGoodsMultipleDocumentTypes))
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

      val paidDuties: Seq[(MRN, Seq[(TaxCode, BigDecimal, Boolean)])] =
        mrns.zip(taxCodesWithAmounts).map { case (mrn, (taxCodes, _, paidAmounts, _)) =>
          (mrn, taxCodes.zip(paidAmounts).map { case (t, r) => (t, r, allDutiesCmaEligible) })
        }

      val reimbursementClaims: OrderedMap[MRN, OrderedMap[TaxCode, Option[BigDecimal]]] =
        OrderedMap.from(
          mrns
            .zip(taxCodesWithAmounts)
            .map { case (mrn, (_, selectedTaxCodes, _, reimbursementAmounts)) =>
              (
                mrn,
                OrderedMap.from(
                  selectedTaxCodes
                    .zip(reimbursementAmounts)
                    .map { case (taxCode, amount) => (taxCode, Option(amount)) }
                )
              )
            }
        )

      val displayDeclarations: Seq[DisplayDeclaration] =
        paidDuties.map { case (mrn, paidDutiesPerMrn) =>
          buildDisplayDeclaration(
            mrn.value,
            declarantEORI,
            if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
            paidDutiesPerMrn,
            if (submitConsigneeDetails) consigneeContact else None,
            declarantContact,
            generateSubsidyPayments = generateSubsidyPayments
          )
        }

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

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
        RejectedGoodsMultipleJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumbers = Some(mrns),
          displayDeclarations = Some(displayDeclarations),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
          contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
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

      RejectedGoodsMultipleJourney.tryBuildFrom(answers, features)
    }

}
