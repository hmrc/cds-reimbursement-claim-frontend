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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils.*

import scala.collection.immutable.SortedMap
import scala.util.Random

/** A collection of generators supporting the tests of RejectedGoodsScheduledClaim. */
object RejectedGoodsScheduledClaimGenerators extends ScheduledClaimGenerators with ClaimTestData {

  val emptyClaim: RejectedGoodsScheduledClaim =
    RejectedGoodsScheduledClaim.empty(exampleEori)

  val claimWithMrnAndDeclaration: RejectedGoodsScheduledClaim =
    RejectedGoodsScheduledClaim
      .empty(exampleImportDeclaration.getDeclarantEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleImportDeclaration)
      .getOrFail

  val completeClaimWithMatchingUserEoriGen: Gen[RejectedGoodsScheduledClaim] =
    Gen.oneOf(
      buildCompleteClaimGen(
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteClaimGen(
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteClaimGen(
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeClaimWithNonNatchingUserEoriGen: Gen[RejectedGoodsScheduledClaim] =
    buildCompleteClaimGen(
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeClaimGen: Gen[RejectedGoodsScheduledClaim] =
    Gen.oneOf(
      completeClaimWithMatchingUserEoriGen,
      completeClaimWithNonNatchingUserEoriGen
    )

  val completeClaimWithOnlySubsidiesGen: Gen[RejectedGoodsScheduledClaim] =
    buildCompleteClaimGen(
      generateSubsidyPayments = GenerateSubsidyPayments.All,
      acc14ConsigneeMatchesUserEori = true,
      acc14DeclarantMatchesUserEori = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false
    )

  val completeClaimWithSomeSubsidiesGen: Gen[RejectedGoodsScheduledClaim] =
    buildCompleteClaimGen(
      generateSubsidyPayments = GenerateSubsidyPayments.Some,
      acc14ConsigneeMatchesUserEori = true,
      acc14DeclarantMatchesUserEori = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false
    )

  val completeClaimGenWithoutSpecialCircumstances: Gen[RejectedGoodsScheduledClaim] = for
    claim        <- completeClaimGen
    basisOfClaim <- Gen.oneOf(BasisOfRejectedGoodsClaim.values - BasisOfRejectedGoodsClaim.SpecialCircumstances)
  yield claim.submitBasisOfClaim(basisOfClaim)

  val completeClaimGenWithSpecialCircumstances: Gen[RejectedGoodsScheduledClaim] = for
    claim                            <- completeClaimGen
    basisOfClaimSpecialCircumstances <- genStringWithMaxSizeOfN(500)
  yield claim
    .submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
    .submitBasisOfClaimSpecialCircumstancesDetails(basisOfClaimSpecialCircumstances)
    .fold(error => throw new Exception(error), identity)

  def buildCompleteClaimGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[RejectedGoodsScheduledClaim.Features] = None,
    payeeType: PayeeType = PayeeType.Consignee
  ): Gen[RejectedGoodsScheduledClaim] =
    buildClaimGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      hasConsigneeDetailsInACC14,
      submitConsigneeDetails = submitConsigneeDetails,
      submitContactDetails = submitContactDetails,
      submitContactAddress = submitContactAddress,
      submitBankAccountType = submitBankAccountType,
      submitBankAccountDetails = submitBankAccountDetails,
      generateSubsidyPayments = generateSubsidyPayments,
      features = features,
      payeeType = Some(payeeType)
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannnot build complete RejectedGoodsScheduledClaim because of $error, fix the test data generator."
          ),
        identity
      )
    )

  def buildClaimGen(
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
    features: Option[RejectedGoodsScheduledClaim.Features] = None,
    payeeType: Option[PayeeType] = None
  ): Gen[Either[String, RejectedGoodsScheduledClaim]] =
    buildAnswersGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      hasConsigneeDetailsInACC14,
      submitDeclarantDetails,
      submitConsigneeDetails,
      submitContactDetails,
      submitContactAddress,
      submitBankAccountDetails,
      submitBankAccountType,
      generateSubsidyPayments,
      payeeType
    )
      .map(RejectedGoodsScheduledClaim.tryBuildFrom(_, features))

  def buildAnswersGen(
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
    payeeType: Option[PayeeType] = None
  ): Gen[RejectedGoodsScheduledClaim.Answers] =
    for
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      declarantEORI               <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      reimbursements              <- dutyTypesWithTaxCodesWithClaimAmountsGen
      basisOfClaim                <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      methodOfDisposal            <- Gen.oneOf(MethodOfDisposal.values)
      numberOfSupportingEvidences <- Gen.choose(1, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <-
        Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.rejectedGoodsScheduledDocumentTypes))
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

      val importDeclaration: ImportDeclaration =
        buildImportDeclaration(
          mrn.value,
          declarantEORI,
          if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
          reimbursements.flatMap(_._2).map(d => (d._1, d._3, false)),
          if submitConsigneeDetails then consigneeContact else None,
          declarantContact,
          generateSubsidyPayments = generateSubsidyPayments
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val correctedAmounts =
        SortedMap
          .from(reimbursements)
          .view
          .mapValues(s =>
            SortedMap(s.map { case (taxCode, paid, correct) =>
              (taxCode, Option(AmountPaidWithCorrect(paid, correct)))
            }*)
          )
          .to(SortedMap)

      val scheduledDocument: UploadedFile =
        buildUploadDocument("schedule").copy(cargo = Some(UploadDocumentType.ScheduleOfMRNs))

      val supportingEvidencesExpanded: Seq[UploadedFile] =
        supportingEvidences.flatMap { case (documentType, size) =>
          (0 until size).map(i => buildUploadDocument(s"$i").copy(cargo = Some(documentType)))
        }.toSeq

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if submitConsigneeDetails && !hasMatchingEori then {
          if submitDeclarantDetails then Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      RejectedGoodsScheduledClaim.Answers(
        nonce = Nonce.random,
        userEoriNumber = userEoriNumber,
        movementReferenceNumber = Some(mrn),
        importDeclaration = Some(importDeclaration),
        payeeType = payeeType,
        eoriNumbersVerification = eoriNumbersVerification,
        contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
        contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
        scheduledDocument = Some(scheduledDocument),
        basisOfClaim = Some(basisOfClaim),
        basisOfClaimSpecialCircumstances =
          if basisOfClaim == BasisOfRejectedGoodsClaim.SpecialCircumstances then Some("special circumstances details")
          else None,
        methodOfDisposal = Some(methodOfDisposal),
        detailsOfRejectedGoods = Some("rejected goods details"),
        correctedAmounts = Some(correctedAmounts),
        exciseCategories = correctedAmounts
          .get(DutyType.Excise)
          .flatMap(
            _.keysIterator.map(_.exciseCategory).collect { case Some(x) => x }.toSeq.distinct.sorted.noneIfEmpty
          ),
        inspectionDate = Some(exampleInspectionDate),
        inspectionAddress = Some(exampleInspectionAddress),
        selectedDocumentType = None,
        supportingEvidences = supportingEvidencesExpanded,
        bankAccountDetails = if submitBankAccountDetails then Some(exampleBankAccountDetails) else None,
        bankAccountType = if submitBankAccountType then Some(bankAccountType) else None,
        modes = ClaimModes(checkYourAnswersChangeMode = true)
      )
    }

  def buildClaimFromAnswersGen(
    answersGen: Gen[RejectedGoodsScheduledClaim.Answers],
    features: Option[RejectedGoodsScheduledClaim.Features] = None
  ): Gen[RejectedGoodsScheduledClaim] =
    answersGen.map(
      RejectedGoodsScheduledClaim
        .tryBuildFrom(_, features)
        .fold(e => throw new Exception(e), identity)
    )

  def answersUpToBasisForClaimGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    forcedTaxCodes: Seq[TaxCode] = Seq.empty
  ): Gen[RejectedGoodsScheduledClaim.Answers] =
    for
      userEoriNumber   <- IdGen.genEori
      mrn              <- IdGen.genMRN
      declarantEORI    <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI    <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes <- Gen.choose(1, 5)
      taxCodes         <- Gen.pick(numberOfTaxCodes, taxCodes).map(_ ++ forcedTaxCodes)
      paidAmounts      <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      consigneeContact <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact <- Gen.option(Acc14Gen.genContactDetails)
    yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val importDeclaration: ImportDeclaration =
        buildImportDeclaration(
          mrn.value,
          declarantEORI,
          if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if submitConsigneeDetails then consigneeContact else None,
          declarantContact = declarantContact
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if submitConsigneeDetails && !hasMatchingEori then {
          if submitDeclarantDetails then Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      RejectedGoodsScheduledClaim.Answers(
        nonce = Nonce.random,
        userEoriNumber = userEoriNumber,
        movementReferenceNumber = Some(mrn),
        importDeclaration = Some(importDeclaration),
        eoriNumbersVerification = eoriNumbersVerification,
        contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
        contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
        modes = ClaimModes(checkYourAnswersChangeMode = false)
      )
    }

  def answersWithDutiesSelectedGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    forcedTaxCodes: Seq[TaxCode] = Seq.empty
  ): Gen[RejectedGoodsScheduledClaim.Answers] =
    for
      userEoriNumber   <- IdGen.genEori
      mrn              <- IdGen.genMRN
      declarantEORI    <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI    <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      reimbursements   <- dutyTypesWithTaxCodesGen
      basisOfClaim     <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      methodOfDisposal <- Gen.oneOf(MethodOfDisposal.values)
      numberOfTaxCodes <- Gen.choose(1, 5)
      taxCodes         <- Gen.pick(numberOfTaxCodes, taxCodes).map(_ ++ forcedTaxCodes)
      paidAmounts      <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      consigneeContact <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact <- Gen.option(Acc14Gen.genContactDetails)
    yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, Random.nextBoolean()) }.toSeq

      val correctedAmounts =
        SortedMap(reimbursements*).view
          .mapValues(s =>
            SortedMap(s.map { taxCode =>
              (taxCode, None)
            }*)
          )
          .to(SortedMap)

      val importDeclaration: ImportDeclaration =
        buildImportDeclaration(
          mrn.value,
          declarantEORI,
          if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if submitConsigneeDetails then consigneeContact else None,
          declarantContact = declarantContact
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if submitConsigneeDetails && !hasMatchingEori then {
          if submitDeclarantDetails then Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        RejectedGoodsScheduledClaim.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          importDeclaration = Some(importDeclaration),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
          contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          methodOfDisposal = Some(methodOfDisposal),
          detailsOfRejectedGoods = Some("rejected goods details"),
          correctedAmounts = Some(correctedAmounts),
          exciseCategories = correctedAmounts
            .get(DutyType.Excise)
            .flatMap(
              _.keysIterator.map(_.exciseCategory).collect { case Some(x) => x }.toSeq.distinct.sorted.noneIfEmpty
            ),
          modes = ClaimModes(checkYourAnswersChangeMode = false)
        )

      answers
    }

}
