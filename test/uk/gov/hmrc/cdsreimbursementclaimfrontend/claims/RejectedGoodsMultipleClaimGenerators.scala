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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EoriNumbersVerification
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimModes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OrderedMap

import scala.jdk.CollectionConverters.*

/** A collection of generators supporting the tests of RejectedGoodsMultipleClaim. */
object RejectedGoodsMultipleClaimGenerators extends ClaimGenerators with ClaimTestData {

  val claimWithMrnAndDeclaration: RejectedGoodsMultipleClaim =
    RejectedGoodsMultipleClaim
      .empty(exampleDisplayDeclaration.getDeclarantEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
      .getOrFail

  def claimWithMrnAndDeclarationWithFeatures(
    features: RejectedGoodsMultipleClaim.Features
  ): RejectedGoodsMultipleClaim =
    RejectedGoodsMultipleClaim
      .empty(exampleDisplayDeclaration.getDeclarantEori, features = Some(features))
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
      .getOrFail

  final val emptyClaim: RejectedGoodsMultipleClaim =
    RejectedGoodsMultipleClaim.empty(exampleEori)

  def incompleteClaimWithMrnsGen(n: Int): Gen[(RejectedGoodsMultipleClaim, Seq[MRN])] = {
    def submitData(claim: RejectedGoodsMultipleClaim)(data: ((MRN, DisplayDeclaration), Int)) =
      claim.submitMovementReferenceNumberAndDeclaration(data._2, data._1._1, data._1._2)

    listOfExactlyN(n, mrnWithDisplayDeclarationGen).map { data =>
      val dataWithIndex: List[((MRN, DisplayDeclaration), Int)] = data.zipWithIndex
      (
        emptyClaim
          .flatMapEach(dataWithIndex, submitData)
          .getOrFail,
        data.map(_._1)
      )
    }
  }

  def incompleteClaimWithMrnsSubsidyOnlyGen(n: Int): Gen[(RejectedGoodsMultipleClaim, Seq[MRN])] = {
    def submitData(claim: RejectedGoodsMultipleClaim)(data: ((MRN, DisplayDeclaration), Int)) = {
      val ((mrn, declaration), index) = data
      if index == 0 then
        claim
          .submitMovementReferenceNumberAndDeclaration(index, mrn, declaration)
          .flatMapWhenDefined(declaration.getConsigneeEori)(_.submitConsigneeEoriNumber)
          .flatMap(_.submitDeclarantEoriNumber(declaration.getDeclarantEori))
      else
        claim
          .submitMovementReferenceNumberAndDeclaration(index, mrn, declaration)
    }

    listOfExactlyN(n, mrnWithDisplayDeclarationSubsidyOnlyGen).map { data =>
      val dataWithIndex: List[((MRN, DisplayDeclaration), Int)] = data.zipWithIndex
      (
        claimWithMrnAndDeclarationWithFeatures(
          RejectedGoodsMultipleClaim.Features()
        )
          .flatMapEach(dataWithIndex, submitData)
          .getOrFail,
        data.map(_._1)
      )
    }
  }

  private def mrnWithSelectedTaxCodesGen(claim: RejectedGoodsMultipleClaim): Seq[Gen[(MRN, Seq[TaxCode])]] =
    claim.answers.movementReferenceNumbers.get.map { mrn =>
      val availableTaxCodes = claim.getAvailableDuties(mrn).map(_._1)
      Gen
        .choose(1, availableTaxCodes.size)
        .map(availableTaxCodes.take)
        .map(seq => (mrn, seq))
    }

  def incompleteClaimWithSelectedDutiesGen(
    n: Int,
    subsidyOnly: Boolean = false
  ): Gen[(RejectedGoodsMultipleClaim, Seq[MRN])] = {
    def submitData(claim: RejectedGoodsMultipleClaim)(data: (MRN, Seq[TaxCode])) =
      claim.selectAndReplaceTaxCodeSetForReimbursement(data._1, data._2)

    val incompleteClaim =
      if subsidyOnly then incompleteClaimWithMrnsSubsidyOnlyGen(n) else incompleteClaimWithMrnsGen(n)
    incompleteClaim.flatMap { case (claim, _) =>
      val gen = mrnWithSelectedTaxCodesGen(claim)
      Gen
        .sequence[Seq[(MRN, Seq[TaxCode])], (MRN, Seq[TaxCode])](gen)
        .map { mrnsWithTaxCodesSelection =>
          val modifiedClaim = claim
            .flatMapEach(mrnsWithTaxCodesSelection, submitData)
            .getOrFail
          (
            modifiedClaim,
            modifiedClaim.answers.movementReferenceNumbers.get
          )
        }
    }
  }

  def incompleteClaimWithCompleteClaimsGen(
    n: Int,
    subsidyOnly: Boolean = false
  ): Gen[(RejectedGoodsMultipleClaim, Seq[MRN])] = {
    def submitData(claim: RejectedGoodsMultipleClaim)(data: (MRN, TaxCode, BigDecimal)) =
      claim.submitClaimAmount(data._1, data._2, data._3)

    incompleteClaimWithSelectedDutiesGen(n, subsidyOnly).map { case (claim, mrns) =>
      val data: Seq[(MRN, TaxCode, BigDecimal)] = mrns.flatMap { mrn =>
        claim.getSelectedDuties(mrn).get.map { taxCode =>
          (mrn, taxCode, BigDecimal(formatAmount(claim.getAmountPaidFor(mrn, taxCode).get)))
        }
      }
      (claim.flatMapEach(data, submitData).getOrFail, mrns)
    }
  }

  val completeClaimWithMatchingUserEoriAndCMAEligibleGen: Gen[RejectedGoodsMultipleClaim] =
    Gen.oneOf(
      buildCompleteClaimGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteClaimGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteClaimGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeClaimWithMatchingUserEoriAndNotCMAEligibleGen: Gen[RejectedGoodsMultipleClaim] =
    Gen.oneOf(
      buildCompleteClaimGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteClaimGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteClaimGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeClaimWithNonNatchingUserEoriAndCMAEligibleGen: Gen[RejectedGoodsMultipleClaim] =
    buildCompleteClaimGen(
      allDutiesCmaEligible = true,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeClaimWithNonNatchingUserEoriAndNotCMAEligibleGen: Gen[RejectedGoodsMultipleClaim] =
    buildCompleteClaimGen(
      allDutiesCmaEligible = false,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeClaimCMAEligibleGen: Gen[RejectedGoodsMultipleClaim] =
    Gen.oneOf(
      completeClaimWithMatchingUserEoriAndCMAEligibleGen,
      completeClaimWithNonNatchingUserEoriAndCMAEligibleGen
    )

  val completeClaimNotCMAEligibleGen: Gen[RejectedGoodsMultipleClaim] =
    Gen.oneOf(
      completeClaimWithMatchingUserEoriAndNotCMAEligibleGen,
      completeClaimWithNonNatchingUserEoriAndNotCMAEligibleGen
    )

  val completeClaimGen: Gen[RejectedGoodsMultipleClaim] =
    Gen.oneOf(
      completeClaimCMAEligibleGen,
      completeClaimNotCMAEligibleGen
    )

  val completeClaimWithOnlySubsidiesGen: Gen[RejectedGoodsMultipleClaim] =
    buildCompleteClaimGen(
      generateSubsidyPayments = GenerateSubsidyPayments.All,
      acc14ConsigneeMatchesUserEori = true,
      acc14DeclarantMatchesUserEori = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false
    )

  val completeClaimWithSomeSubsidiesGen: Gen[RejectedGoodsMultipleClaim] =
    buildCompleteClaimGen(
      generateSubsidyPayments = GenerateSubsidyPayments.Some,
      acc14ConsigneeMatchesUserEori = true,
      acc14DeclarantMatchesUserEori = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false
    )

  val completeClaimGenWithoutSpecialCircumstances: Gen[RejectedGoodsMultipleClaim] = for
    claim        <- completeClaimGen
    basisOfClaim <- Gen.oneOf(BasisOfRejectedGoodsClaim.values - BasisOfRejectedGoodsClaim.SpecialCircumstances)
  yield claim.submitBasisOfClaim(basisOfClaim)

  val completeClaimGenWithSpecialCircumstances: Gen[RejectedGoodsMultipleClaim] = for
    claim                            <- completeClaimGen
    basisOfClaimSpecialCircumstances <- genStringWithMaxSizeOfN(500)
  yield claim
    .submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
    .submitBasisOfClaimSpecialCircumstancesDetails(basisOfClaimSpecialCircumstances)
    .fold(error => throw new Exception(error), identity)

  def buildCompleteClaimGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitPayeeType: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[RejectedGoodsMultipleClaim.Features] = None
  ): Gen[RejectedGoodsMultipleClaim] =
    buildClaimGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      allDutiesCmaEligible,
      hasConsigneeDetailsInACC14,
      submitConsigneeDetails = submitConsigneeDetails,
      submitContactDetails = submitContactDetails,
      submitContactAddress = submitContactAddress,
      submitPayeeType = submitPayeeType,
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
            s"Cannnot build complete RejectedGoodsMultipleClaim because of $error, fix the test data generator."
          ),
        identity
      )
    )

  type TaxCodesAndAmounts = (Seq[TaxCode], Seq[TaxCode], List[BigDecimal], Seq[BigDecimal])

  def taxCodesAndAmountsGen(maxSize: Int): Gen[TaxCodesAndAmounts] = for
    numberOfTaxCodes         <- Gen.choose(1, maxSize)
    numberOfSelectedTaxCodes <- Gen.choose(1, numberOfTaxCodes)
    taxCodes                 <- Gen.pick(numberOfTaxCodes, TaxCodes.all).map(_.distinct)
    paidAmounts              <- Gen.listOfN(numberOfTaxCodes, amountNumberGen)
    correctedAmounts         <-
      Gen
        .sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts
            .take(numberOfSelectedTaxCodes)
            .map(a => Gen.choose(BigDecimal("0.00"), a - BigDecimal.exact("0.01")))
        )
  yield (taxCodes.toSeq, taxCodes.take(numberOfSelectedTaxCodes).toSeq, paidAmounts, correctedAmounts)

  def buildClaimGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitPayeeType: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[RejectedGoodsMultipleClaim.Features] = None
  ): Gen[Either[String, RejectedGoodsMultipleClaim]] =
    for
      userEoriNumber      <- IdGen.genEori
      numberOfMRNs        <- Gen.choose(minNumberOfMRNs, Math.max(minNumberOfMRNs, maxNumberOfMRNs))
      mrns                <- Gen.listOfN(numberOfMRNs, IdGen.genMRN)
      declarantEORI       <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI       <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      taxCodesWithAmounts <- Gen.sequence(mrns.map(_ => taxCodesAndAmountsGen(maxSize))).map(_.asScala)
      basisOfClaim        <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      methodOfDisposal    <- Gen.oneOf(MethodOfDisposal.values)
      payeeType           <- Gen.oneOf(PayeeType.values)

      numberOfSupportingEvidences <- Gen.choose(1, maxSize)
      numberOfDocumentTypes       <- Gen.choose(1, maxSize - 1)
      documentTypes               <-
        Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.rejectedGoodsMultipleDocumentTypes))
      supportingEvidences         <-
        Gen
          .sequence[Seq[(UploadDocumentType, Int)], (UploadDocumentType, Int)](
            documentTypes.map(dt => Gen.choose(1, numberOfSupportingEvidences).map(n => (dt, n)))
          )
          .map(_.toMap)
      bankAccountType             <- Gen.oneOf(BankAccountType.values)
      consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
    yield {

      val paidDuties: Seq[(MRN, Seq[(TaxCode, BigDecimal, Boolean)])] =
        mrns.zip(taxCodesWithAmounts).map { case (mrn, (taxCodes, _, paidAmounts, _)) =>
          (mrn, taxCodes.zip(paidAmounts).map { case (t, r) => (t, r, allDutiesCmaEligible) })
        }

      val correctedAmounts: OrderedMap[MRN, OrderedMap[TaxCode, Option[BigDecimal]]] =
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
            if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
            paidDutiesPerMrn,
            if submitConsigneeDetails then consigneeContact else None,
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
        if submitConsigneeDetails && !hasMatchingEori then {
          if submitDeclarantDetails then Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        RejectedGoodsMultipleClaim.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          payeeType = if submitPayeeType then Some(payeeType) else None,
          movementReferenceNumbers = Some(mrns),
          displayDeclarations = Some(displayDeclarations),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
          contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          basisOfClaimSpecialCircumstances =
            if basisOfClaim == BasisOfRejectedGoodsClaim.SpecialCircumstances then Some("special circumstances details")
            else None,
          methodOfDisposal = Some(methodOfDisposal),
          detailsOfRejectedGoods = Some("rejected goods details"),
          correctedAmounts = Some(correctedAmounts),
          inspectionDate = Some(exampleInspectionDate),
          inspectionAddress = Some(exampleInspectionAddress),
          selectedDocumentType = None,
          supportingEvidences = supportingEvidencesExpanded,
          bankAccountDetails = if submitBankAccountDetails then Some(exampleBankAccountDetails) else None,
          bankAccountType = if submitBankAccountType then Some(bankAccountType) else None,
          modes = ClaimModes(checkYourAnswersChangeMode = true)
        )

      RejectedGoodsMultipleClaim.tryBuildFrom(answers, features)
    }

}
