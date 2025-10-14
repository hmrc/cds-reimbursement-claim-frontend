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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator
import org.scalacheck.Gen
import org.scalacheck.ShrinkLowPriority
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimValidationErrors.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*

class RejectedGoodsScheduledClaimSpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "RejectedGoodsScheduledClaim" should {
    "have an empty instance" in {
      emptyClaim.answers.userEoriNumber                                         shouldBe exampleEori
      emptyClaim.answers.bankAccountDetails                                     shouldBe None
      emptyClaim.answers.bankAccountType                                        shouldBe None
      emptyClaim.answers.basisOfClaim                                           shouldBe None
      emptyClaim.answers.basisOfClaimSpecialCircumstances                       shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.contactDetails                                         shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.eoriNumbersVerification                                shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber) shouldBe None
      emptyClaim.answers.detailsOfRejectedGoods                                 shouldBe None
      emptyClaim.answers.importDeclaration                                      shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyClaim.answers.inspectionAddress                                      shouldBe None
      emptyClaim.answers.inspectionDate                                         shouldBe None
      emptyClaim.answers.methodOfDisposal                                       shouldBe None
      emptyClaim.answers.correctedAmounts                                       shouldBe None
      emptyClaim.answers.selectedDocumentType                                   shouldBe None
      emptyClaim.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyClaim.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyClaim.getSelectedDutyTypes                                           shouldBe None
      emptyClaim.getReimbursementClaims                                         shouldBe Map.empty
      emptyClaim.hasCompleteReimbursementClaims                                 shouldBe false
      emptyClaim.hasCompleteSupportingEvidences                                 shouldBe false
      emptyClaim.hasCompleteAnswers                                             shouldBe false
      emptyClaim.toOutput.isLeft                                                shouldBe true
      emptyClaim.isFinalized                                                    shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeClaimGen) { claim =>
        RejectedGoodsScheduledClaim.validator.apply(claim) shouldBe Right(())
        claim.answers.checkYourAnswersChangeMode           shouldBe true
        claim.hasCompleteReimbursementClaims               shouldBe true
        claim.hasCompleteSupportingEvidences               shouldBe true
        claim.hasCompleteAnswers                           shouldBe true
        claim.isFinalized                                  shouldBe false

        val output = claim.toOutput.getOrElse(fail("Claim output not defined."))

        output.movementReferenceNumber  shouldBe claim.answers.movementReferenceNumber.get
        output.claimantType             shouldBe claim.getClaimantType
        output.basisOfClaim             shouldBe claim.answers.basisOfClaim.get
        output.methodOfDisposal         shouldBe claim.answers.methodOfDisposal.get
        output.detailsOfRejectedGoods   shouldBe claim.answers.detailsOfRejectedGoods.get
        output.inspectionDate           shouldBe claim.answers.inspectionDate.get
        output.inspectionAddress        shouldBe claim.answers.inspectionAddress.get
        output.reimbursementMethod      shouldBe ReimbursementMethod.BankAccountTransfer
        output.reimbursementClaims      shouldBe claim.getReimbursementClaims
        output.scheduledDocument        shouldBe EvidenceDocument.from(claim.answers.scheduledDocument.get)
        output.supportingEvidences.size shouldBe claim.answers.supportingEvidences.size
        output.bankAccountDetails       shouldBe claim.answers.bankAccountDetails
        output.claimantInformation.eori shouldBe claim.answers.userEoriNumber
      }
    }

    "finalize claim with caseNumber" in {
      forAll(completeClaimGen) { claim =>
        claim.hasCompleteReimbursementClaims shouldBe true
        claim.hasCompleteSupportingEvidences shouldBe true
        claim.hasCompleteAnswers             shouldBe true
        claim.isFinalized                    shouldBe false
        val result        = claim.finalizeClaimWith("foo-123-abc")
        val modifiedClaim = result.getOrFail
        modifiedClaim.isFinalized                    shouldBe true
        modifiedClaim.hasCompleteReimbursementClaims shouldBe true
        modifiedClaim.hasCompleteSupportingEvidences shouldBe true
        modifiedClaim.hasCompleteAnswers             shouldBe true
        modifiedClaim.finalizeClaimWith("bar")       shouldBe Left(CLAIM_ALREADY_FINALIZED)
      }
    }

    "accept submission of a new MRN" in {
      forAll(mrnWithImportDeclarationGen) { case (mrn, decl) =>
        val claim = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(mrn, decl)
          .getOrFail
        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteReimbursementClaims                shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.isFinalized                                   shouldBe false
      }
    }

    "decline submission of a wrong display declaration" in {
      forAll(mrnWithImportDeclarationGen) { case (mrn, decl) =>
        val claimEither = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(mrn, decl.withDeclarationId("foo"))

        claimEither shouldBe Left("submitMovementReferenceNumber.wrongImportDeclarationMrn")
      }
    }

    "accept change of the MRN" in {
      forAll(completeClaimGen, importDeclarationGen) { (claim, decl) =>
        val decl2         = decl.withDeclarationId(exampleMrnAsString)
        val modifiedClaim = claim
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, decl2)
          .getOrFail
        modifiedClaim.answers.importDeclaration      shouldBe Some(decl2)
        modifiedClaim.hasCompleteAnswers             shouldBe false
        modifiedClaim.hasCompleteReimbursementClaims shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences shouldBe false
      }
    }

    "accept change of the MRN when user has XI eori" in {
      forAll(
        completeClaimGen.map(_.submitUserXiEori(UserXiEori(exampleXIEori.value))),
        importDeclarationGen
      ) { (claim, decl) =>
        val decl2         = decl.withDeclarationId(exampleMrnAsString)
        val modifiedClaim = claim
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, decl2)
          .getOrFail
        modifiedClaim.answers.importDeclaration       shouldBe Some(decl2)
        modifiedClaim.hasCompleteAnswers              shouldBe false
        modifiedClaim.hasCompleteReimbursementClaims  shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences  shouldBe false
        modifiedClaim.answers.eoriNumbersVerification shouldBe Some(
          EoriNumbersVerification(userXiEori = Some(UserXiEori(exampleXIEori.value)))
        )
      }
    }

    "accept submission of the same MRN" in {
      forAll(completeClaimGen) { claim =>
        val modifiedClaim = claim
          .submitMovementReferenceNumberAndDeclaration(
            claim.answers.movementReferenceNumber.get,
            claim.answers.importDeclaration.get
          )
          .getOrFail
        modifiedClaim                                shouldBe claim
        modifiedClaim.hasCompleteAnswers             shouldBe true
        modifiedClaim.hasCompleteReimbursementClaims shouldBe true
        modifiedClaim.hasCompleteSupportingEvidences shouldBe true
      }
    }

    "accept submission of a new ACC14 data" in {
      forAll(importDeclarationGen) { acc14 =>
        val claim = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(
            exampleMrn,
            acc14.withDeclarationId(exampleMrnAsString)
          )
          .getOrFail

        claim.answers.movementReferenceNumber.contains(exampleMrn)                            shouldBe true
        claim.answers.importDeclaration.contains(acc14.withDeclarationId(exampleMrnAsString)) shouldBe true
        claim.hasCompleteAnswers                                                              shouldBe false
        claim.hasCompleteReimbursementClaims                                                  shouldBe false
        claim.hasCompleteSupportingEvidences                                                  shouldBe false
      }
    }

    "accept change of the ACC14 data" in {
      forAll(completeClaimGen) { claim =>
        val modifiedClaim =
          claim
            .submitMovementReferenceNumberAndDeclaration(
              exampleMrn,
              exampleImportDeclaration
            )
            .getOrFail
        modifiedClaim.answers.movementReferenceNumber shouldBe Some(exampleMrn)
        modifiedClaim.answers.importDeclaration       shouldBe Some(exampleImportDeclaration)
        modifiedClaim.answers.correctedAmounts        shouldBe None
        modifiedClaim.hasCompleteAnswers              shouldBe false
        modifiedClaim.hasCompleteReimbursementClaims  shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences  shouldBe false
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val claim             =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe true
      claim.getClaimantType                          shouldBe ClaimantType.User
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "needs XI eori submission if user's eori not matching those of ACC14 and ACC14 contains XI eori" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val claim             =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .getOrFail

      exampleXIEori.isXiEori                         shouldBe true
      anotherExampleEori.isXiEori                    shouldBe false
      claim.userHasGBEoriMatchingDeclaration         shouldBe false
      claim.userHasXIEoriMatchingDeclaration         shouldBe false
      claim.needsUserXiEoriSubmission                shouldBe true
      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe true

      val claim2 = claim.submitUserXiEori(UserXiEori(exampleXIEori.value))

      claim2.userHasGBEoriMatchingDeclaration         shouldBe false
      claim2.userHasXIEoriMatchingDeclaration         shouldBe true
      claim2.needsUserXiEoriSubmission                shouldBe false
      claim2.needsDeclarantAndConsigneeEoriSubmission shouldBe false
    }

    "does not need declarant and consignee submission if user's eori is matching that of declarant" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleEori, consigneeEORI = None)
      val claim             =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's eori is matching that of consignee" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleEori))
      val claim             =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Consignee
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is missing" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleXIEori, consigneeEORI = None)
      val claim             =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is present" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleXIEori, consigneeEORI = Some(anotherExampleEori))
      val claim             =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of consignee" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val claim             =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Consignee
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "fail building claim if user's eori is not matching those of ACC14 and separate EORIs were not provided by the user" in {
      val claimGen = buildClaimGen(
        acc14DeclarantMatchesUserEori = false,
        acc14ConsigneeMatchesUserEori = false,
        submitDeclarantDetails = false,
        submitConsigneeDetails = false
      )
      forAll(claimGen) { result =>
        val claim = result.getOrElse(fail("Claim building has failed."))
        claim.hasCompleteAnswers shouldBe false
      }
    }

    "fail if submitted consignee EORI is not needed" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleEori)
      val claimEither       =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori)
      val claimEither       =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "fail if submitted declarant EORI is not needed" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleEori)
      val claimEither       =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori)
      val claimEither       =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val claim =
          RejectedGoodsScheduledClaim
            .empty(exampleEori)
            .submitContactDetails(Some(contactDetails))

        claim.answers.contactDetails shouldBe Some(contactDetails)
      }
    }

    "change contact details" in {
      forAll(completeClaimGen, ContactDetailsGen.genMrnContactDetails) { (claim, contactDetails) =>
        val modifiedClaim = claim.submitContactDetails(Some(contactDetails))

        modifiedClaim.hasCompleteAnswers     shouldBe true
        modifiedClaim.answers.contactDetails shouldBe Some(
          contactDetails
        )
      }
    }

    "submit contact address" in {
      forAll(ContactAddressGen.genContactAddress) { contactAddress =>
        val claim = RejectedGoodsScheduledClaim.empty(exampleEori).submitContactAddress(contactAddress)

        claim.answers.contactAddress shouldBe Some(contactAddress)
      }
    }

    "change contact address" in {
      forAll(completeClaimGen, ContactAddressGen.genContactAddress) { (claim, contactAddress) =>
        val modifiedClaim = claim.submitContactAddress(contactAddress)

        modifiedClaim.hasCompleteAnswers     shouldBe true
        modifiedClaim.answers.contactAddress shouldBe Some(
          contactAddress.computeChanges(claim.getInitialAddressDetailsFromDeclaration)
        )
      }
    }

    "submit basis of claim" in {
      forAll(Gen.oneOf(BasisOfRejectedGoodsClaim.values)) { basisOfClaim =>
        val claim = RejectedGoodsScheduledClaim.empty(exampleEori).submitBasisOfClaim(basisOfClaim)
        claim.answers.basisOfClaim shouldBe Some(basisOfClaim)
      }
    }

    "change basis of claim" in {
      forAll(completeClaimGen, Gen.oneOf(BasisOfRejectedGoodsClaim.allButSpecialCircumstances)) {
        (claim, basisOfClaim) =>
          val modifiedClaim = claim.submitBasisOfClaim(basisOfClaim)

          modifiedClaim.hasCompleteAnswers           shouldBe true
          modifiedClaim.toOutput.map(_.basisOfClaim) shouldBe Right(basisOfClaim)
      }
    }

    "change basis of claim if special circumstances" in {
      forAll(completeClaimGenWithoutSpecialCircumstances) { claim =>
        val modifiedClaim = claim.submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)

        modifiedClaim.hasCompleteAnswers           shouldBe false
        modifiedClaim.toOutput.map(_.basisOfClaim) shouldBe Left(
          BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_BE_DEFINED :: Nil
        )
      }
    }

    "submit basis of claim special circumstances details" in {
      val claimEither = RejectedGoodsScheduledClaim
        .empty(exampleEori)
        .submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
        .submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails)

      claimEither.isRight shouldBe true

    }

    "change basis of claim special circumstances details" in {
      forAll(completeClaimGen) { claim =>
        val modifiedClaimEither =
          claim.submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails)

        modifiedClaimEither.isRight shouldBe claim.needsSpecialCircumstancesBasisOfClaim
      }
    }

    "submit method of disposal" in {
      forAll(Gen.oneOf(MethodOfDisposal.values)) { methodOfDisposal =>
        val claim = RejectedGoodsScheduledClaim.empty(exampleEori).submitMethodOfDisposal(methodOfDisposal)
        claim.answers.methodOfDisposal shouldBe Some(methodOfDisposal)
      }
    }

    "change method of disposal" in {
      forAll(completeClaimGen, Gen.oneOf(MethodOfDisposal.values)) { (claim, methodOfDisposal) =>
        val modifiedClaim = claim.submitMethodOfDisposal(methodOfDisposal)

        modifiedClaim.hasCompleteAnswers               shouldBe true
        modifiedClaim.toOutput.map(_.methodOfDisposal) shouldBe Right(methodOfDisposal)
      }
    }

    "submit details of rejected goods" in {
      val claim = RejectedGoodsScheduledClaim
        .empty(exampleEori)
        .submitDetailsOfRejectedGoods(exampleRejectedGoodsDetails)

      claim.answers.detailsOfRejectedGoods shouldBe Some(exampleRejectedGoodsDetails)
    }

    "change details of rejected goods" in {
      forAll(completeClaimGen, exampleRejectedGoodsDetails) { (claim, rejectedGoodsDetails) =>
        val modifiedClaim = claim.submitDetailsOfRejectedGoods(rejectedGoodsDetails)

        modifiedClaim.hasCompleteAnswers                     shouldBe true
        modifiedClaim.toOutput.map(_.detailsOfRejectedGoods) shouldBe Right(rejectedGoodsDetails)
      }
    }

    "select duty types for reimbursement when none yet selected" in {
      forAll(dutyTypesGen) { (dutyTypes: Seq[DutyType]) =>
        val claim = RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .getOrFail

        claim.getSelectedDutyTypes shouldBe Some(dutyTypes)
      }
    }

    "replace duty types for reimbursement" in {
      forAll(dutyTypesGen, dutyTypesGen) { (dutyTypes1: Seq[DutyType], dutyTypes2: Seq[DutyType]) =>
        val claim = RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes1)
          .flatMap(_.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes2))
          .getOrFail

        claim.getSelectedDutyTypes shouldBe Some(dutyTypes2)
      }
    }

    "select valid tax codes for reimbursement when none yet selected" in {
      forAll(dutyTypesWithTaxCodesGen) { (dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]) =>
        val dutyTypes = dutyTypesWithTaxCodes.map(_._1)
        val claim     = RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            claim => (tc: (DutyType, Seq[TaxCode])) => claim.selectAndReplaceTaxCodeSetForDutyType(tc._1, tc._2)
          )
          .getOrFail

        claim.getSelectedDutyTypes shouldBe Some(dutyTypes)
        dutyTypesWithTaxCodes.foreach { case (dutyType, taxCodes) =>
          claim.getSelectedDutiesFor(dutyType) shouldBe Some(taxCodes)
          taxCodes.foreach(taxCode => claim.isDutySelected(dutyType, taxCode))
          TaxCodes.allExcept(taxCodes.toSet).foreach(taxCode => !claim.isDutySelected(dutyType, taxCode))
        }
      }
    }

    "replace valid tax codes for reimbursement" in {
      DutyTypes.all.foreach { dutyType =>
        forAll(taxCodesGen(dutyType), taxCodesGen(dutyType)) { (taxCodes1, taxCodes2) =>
          val claim = RejectedGoodsScheduledClaim
            .empty(exampleEori)
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .flatMapEach(
              Seq(dutyType -> taxCodes1),
              claim => (tc: (DutyType, Seq[TaxCode])) => claim.selectAndReplaceTaxCodeSetForDutyType(tc._1, tc._2)
            )
            .flatMapEach(
              Seq(dutyType -> taxCodes2),
              claim => (tc: (DutyType, Seq[TaxCode])) => claim.selectAndReplaceTaxCodeSetForDutyType(tc._1, tc._2)
            )
            .getOrFail

          claim.getSelectedDutyTypes.get       shouldBe Seq(dutyType)
          claim.getSelectedDutiesFor(dutyType) shouldBe Some(taxCodes2)
          taxCodes2.foreach(taxCode => claim.isDutySelected(dutyType, taxCode))
          TaxCodes.allExcept(taxCodes2.toSet).foreach(taxCode => !claim.isDutySelected(dutyType, taxCode))
        }
      }
    }

    "select invalid tax codes for reimbursement" in {
      forAll(dutyTypesWithTaxCodesGen) { (dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]) =>
        val dutyTypes = dutyTypesWithTaxCodes.map(_._1)
        val result    = RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            claim =>
              (tc: (DutyType, Seq[TaxCode])) =>
                claim.selectAndReplaceTaxCodeSetForDutyType(tc._1, Seq(TaxCode.A00, TaxCode.A50))
          )

        result shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesDoesNotMatchDutyType")
      }
    }

    "select invalid duty types for reimbursement" in {
      forAll(dutyTypesWithTaxCodesGen) { (dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]) =>
        val dutyTypes = dutyTypesWithTaxCodes.map(_._1)
        val result    = RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(DutyTypes.all.toSet.diff(dutyTypes.toSet).toSeq)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            claim => (tc: (DutyType, Seq[TaxCode])) => claim.selectAndReplaceTaxCodeSetForDutyType(tc._1, tc._2)
          )

        result shouldBe Left("selectTaxCodeSetForReimbursement.dutyTypeNotSelectedBefore")
      }
    }

    "change duty types for reimbursement with the same set" in {
      forAll(completeClaimGen) { claim =>
        val result = claim
          .selectAndReplaceDutyTypeSetForReimbursement(claim.getSelectedDutyTypes.get)
          .getOrFail
        result.hasCompleteReimbursementClaims shouldBe true
        result.hasCompleteAnswers             shouldBe true
      }
    }

    def isSubset[A](a: Seq[A], b: Seq[A]): Boolean =
      a.toSet.intersect(b.toSet) === a.toSet

    "change duty types for reimbursement with a new valid set" in {
      forAll(completeClaimGen, dutyTypesGen) { (claim, newDutyTypes) =>
        val result = claim
          .selectAndReplaceDutyTypeSetForReimbursement(newDutyTypes)
          .getOrFail
        result.getSelectedDutyTypes.get       shouldBe newDutyTypes
        result.hasCompleteReimbursementClaims shouldBe isSubset(newDutyTypes, claim.getSelectedDutyTypes.get)
      }
    }

    "change tax codes for reimbursement with the same set" in {
      forAll(completeClaimGen) { claim =>
        claim.getSelectedDutyTypes.get.foreach { dutyType =>
          val taxCodes = claim.getSelectedDutiesFor(dutyType).get
          val result   = claim
            .selectAndReplaceTaxCodeSetForDutyType(dutyType, taxCodes)
            .getOrFail
          result.hasCompleteReimbursementClaims shouldBe true
          result.hasCompleteAnswers             shouldBe true
        }
      }
    }

    "change tax codes for reimbursement with the same sets" in {
      forAll(completeClaimGen) { claim =>
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])] =
          claim.getSelectedDutyTypes.get.map(dutyType => dutyType -> claim.getSelectedDutiesFor(dutyType).get)

        val result = claim
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForDutyType(d._1, d._2)
          )
          .getOrFail

        result.hasCompleteReimbursementClaims shouldBe true
        result.hasCompleteAnswers             shouldBe true
      }
    }

    "submit valid correct amounts for selected duty types and tax codes" in {
      forAll(dutyTypesWithTaxCodesWithClaimAmountsGen) { data =>
        val dutyTypes: Seq[DutyType]                                              = data.map(_._1)
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                  = data.map { case (dt, tcs) => dt -> tcs.map(_._1) }
        val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)] = data.flatMap {
          case (dutyType, tca) =>
            tca.map { case (taxCode, paidAmount, correctAmount) => (dutyType, taxCode, paidAmount, correctAmount) }
        }
        val expectedTotalReimbursementAmount                                      =
          taxCodesWithAmounts.map { case (_, _, paidAmount, correctAmount) => paidAmount - correctAmount }.sum

        val claim = RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            (j: RejectedGoodsScheduledClaim) =>
              { case (dutyType: DutyType, taxCodeSeq: Seq[TaxCode]) =>
                j.selectAndReplaceTaxCodeSetForDutyType(dutyType, taxCodeSeq)
              }: (((DutyType, Seq[TaxCode])) => Either[String, RejectedGoodsScheduledClaim])
          )
          .flatMapEach(
            taxCodesWithAmounts,
            (j: RejectedGoodsScheduledClaim) =>
              { case (dutyType: DutyType, taxCode: TaxCode, paidAmount: BigDecimal, correctAmount: BigDecimal) =>
                j.submitCorrectAmount(dutyType, taxCode, paidAmount, correctAmount)
              }: (((DutyType, TaxCode, BigDecimal, BigDecimal)) => Either[String, RejectedGoodsScheduledClaim])
          )
          .getOrFail

        claim.getSelectedDutyTypes shouldBe Some(dutyTypes)

        dutyTypesWithTaxCodes.foreach { case (dutyType, taxCodes) =>
          claim.getSelectedDutiesFor(dutyType).get shouldBe taxCodes
          taxCodes.foreach(taxCode => claim.isDutySelected(dutyType, taxCode))
          TaxCodes.allExcept(taxCodes.toSet).foreach(taxCode => !claim.isDutySelected(dutyType, taxCode))
        }

        claim.getReimbursementClaims.map(_._2.size).sum shouldBe taxCodesWithAmounts.size
        claim.getTotalReimbursementAmount               shouldBe expectedTotalReimbursementAmount
      }
    }

    "submit valid claim amounts for selected duty types and tax codes" in {
      forAll(dutyTypesWithTaxCodesWithClaimAmountsGen) { data =>
        val dutyTypes: Seq[DutyType]                                              = data.map(_._1)
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                  = data.map { case (dt, tcs) => dt -> tcs.map(_._1) }
        val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)] = data.flatMap {
          case (dutyType, tca) =>
            tca.map { case (taxCode, paidAmount, correctAmount) => (dutyType, taxCode, paidAmount, correctAmount) }
        }
        val expectedTotalReimbursementAmount                                      =
          taxCodesWithAmounts.map { case (_, _, paidAmount, correctAmount) => paidAmount - correctAmount }.sum

        val claim = RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            (j: RejectedGoodsScheduledClaim) =>
              { case (dutyType: DutyType, taxCodeSeq: Seq[TaxCode]) =>
                j.selectAndReplaceTaxCodeSetForDutyType(dutyType, taxCodeSeq)
              }: (((DutyType, Seq[TaxCode])) => Either[String, RejectedGoodsScheduledClaim])
          )
          .flatMapEach(
            taxCodesWithAmounts,
            (j: RejectedGoodsScheduledClaim) =>
              { case (dutyType: DutyType, taxCode: TaxCode, paidAmount: BigDecimal, correctAmount: BigDecimal) =>
                j.submitClaimAmount(dutyType, taxCode, paidAmount, paidAmount - correctAmount)
              }: (((DutyType, TaxCode, BigDecimal, BigDecimal)) => Either[String, RejectedGoodsScheduledClaim])
          )
          .getOrFail

        claim.getSelectedDutyTypes shouldBe Some(dutyTypes)

        dutyTypesWithTaxCodes.foreach { case (dutyType, taxCodes) =>
          claim.getSelectedDutiesFor(dutyType).get shouldBe taxCodes
          taxCodes.foreach(taxCode => claim.isDutySelected(dutyType, taxCode))
          TaxCodes.allExcept(taxCodes.toSet).foreach(taxCode => !claim.isDutySelected(dutyType, taxCode))
        }

        claim.getReimbursementClaims.map(_._2.size).sum shouldBe taxCodesWithAmounts.size
        claim.getTotalReimbursementAmount               shouldBe expectedTotalReimbursementAmount
      }
    }

    "reject submit valid amount for tax code not matching duty type" in {
      forAll(dutyTypesWithTaxCodesWithClaimAmountsGen) { data =>
        val dutyTypes: Seq[DutyType]                                              = data.map(_._1)
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                  = data.map { case (dt, tcs) => dt -> tcs.map(_._1) }
        val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)] = data.flatMap { case (dt, tca) =>
          tca.map { case (tc, pa, ca) => (dt, tc, pa, ca) }
        }

        def taxCodeNotMatchingDutyType(dutyType: DutyType): TaxCode =
          DutyTypes.all.iterator.filterNot(_ === dutyType).next().taxCodes.iterator.next()

        val result = RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForDutyType(d._1, d._2)
          )
          .flatMapEach(
            taxCodesWithAmounts,
            j =>
              (d: (DutyType, TaxCode, BigDecimal, BigDecimal)) =>
                j.submitCorrectAmount(d._1, taxCodeNotMatchingDutyType(d._1), d._3, d._4)
          )

        result shouldBe Left("submitAmountForReimbursement.taxCodeNotMatchingDutyType")
      }
    }

    "reject submit valid amount for not selected tax code" in {
      forAll(dutyTypesWithTaxCodesWithClaimAmountsGen) { data =>
        val dutyTypes: Seq[DutyType]                                              = data.map(_._1)
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                  = data.map { case (dt, tcs) => dt -> tcs.map(_._1) }
        val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)] = data.flatMap { case (dt, tca) =>
          tca.map { case (tc, pa, ca) => (dt, tc, pa, ca) }
        }

        def taxCodeNotSelected(dutyType: DutyType): TaxCode =
          dutyTypesWithTaxCodes
            .find(_._1 === dutyType)
            .flatMap { case (dt, tcs) =>
              dt.taxCodes.find(tc => !tcs.contains(tc))
            }
            .getOrElse(fail())

        val result = RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForDutyType(d._1, d._2)
          )
          .flatMapEach(
            taxCodesWithAmounts,
            j =>
              (d: (DutyType, TaxCode, BigDecimal, BigDecimal)) =>
                j.submitCorrectAmount(d._1, taxCodeNotSelected(d._1), d._3, d._4)
          )

        result shouldBe Left("submitAmountForReimbursement.taxCodeNotSelected")
      }
    }

    "reject submit invalid amount for valid selected tax code" in {
      forAll(dutyTypesWithTaxCodesWithClaimAmountsGen) { data =>
        val dutyTypes: Seq[DutyType]                                              = data.map(_._1)
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                  = data.map { case (dt, tcs) => dt -> tcs.map(_._1) }
        val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)] = data.flatMap { case (dt, tca) =>
          tca.map { case (tc, pa, ca) => (dt, tc, pa, ca) }
        }

        val result = RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForDutyType(d._1, d._2)
          )
          .flatMapEach(
            taxCodesWithAmounts,
            j =>
              (d: (DutyType, TaxCode, BigDecimal, BigDecimal)) =>
                j.submitCorrectAmount(d._1, d._2, d._4, d._3) // swaped amounts
          )
        result shouldBe Left("submitAmountForReimbursement.invalidReimbursementAmount")
      }
    }

    "change to valid amount for valid selected tax code" in {
      forAll(completeClaimGen) { claim =>
        val totalReimbursementAmount = claim.getTotalReimbursementAmount
        val totalPaidAmount          = claim.getTotalPaidAmount

        claim.getReimbursementClaims.foreachEntry { case (dutyType, tca) =>
          tca.foreachEntry { case (taxCode, AmountPaidWithCorrect(pa, ca)) =>
            val modifiedClaim =
              claim.submitCorrectAmount(dutyType, taxCode, pa * 0.8, ca * 0.8).getOrFail

            modifiedClaim.getTotalReimbursementAmount shouldBe totalReimbursementAmount - ((pa - ca) * 0.2)
            modifiedClaim.getTotalPaidAmount          shouldBe totalPaidAmount - pa * 0.2

          }
        }
      }
    }

    "reject change to valid amount for not selected tax code" in {
      forAll(completeClaimGen) { claim =>
        def taxCodeNotSelected(dutyType: DutyType): TaxCode =
          claim
            .getSelectedDutiesFor(dutyType)
            .map(tcs => dutyType.taxCodes.filterNot(tcs.contains).iterator.next())
            .getOrElse(fail())

        claim.getReimbursementClaims.foreachEntry { case (dutyType, tca) =>
          tca.foreachEntry { case (_, AmountPaidWithCorrect(pa, ca)) =>
            val result =
              claim.submitCorrectAmount(dutyType, taxCodeNotSelected(dutyType), pa, ca)
            result shouldBe Left("submitAmountForReimbursement.taxCodeNotSelected")
          }
        }
      }
    }

    "reject change to invalid amount for valid selected tax code" in {
      forAll(completeClaimGen) { claim =>
        claim.getReimbursementClaims.foreachEntry { case (dutyType, tca) =>
          tca.foreachEntry { case (taxCode, AmountPaidWithCorrect(pa, _)) =>
            val result1 =
              claim.submitCorrectAmount(dutyType, taxCode, pa, pa)
            result1 shouldBe Left("submitAmountForReimbursement.invalidReimbursementAmount")

            val result2 =
              claim.submitCorrectAmount(dutyType, taxCode, pa, BigDecimal("-0.01"))
            result2 shouldBe Left("submitAmountForReimbursement.invalidReimbursementAmount")

            val result3 =
              claim.submitCorrectAmount(dutyType, taxCode, pa, pa + BigDecimal("0.01"))
            result3 shouldBe Left("submitAmountForReimbursement.invalidReimbursementAmount")
          }
        }
      }
    }

    "submit inspection date" in {
      forAll(DateGen.genDate) { inspectionDate =>
        val claim = RejectedGoodsScheduledClaim.empty(exampleEori).submitInspectionDate(inspectionDate)

        claim.answers.inspectionDate shouldBe Some(inspectionDate)

      }
    }

    "change inspection date" in {
      forAll(completeClaimGen, DateGen.genDate) { (claim, inspectionDate) =>
        val modifiedClaim = claim.submitInspectionDate(inspectionDate)

        modifiedClaim.hasCompleteAnswers             shouldBe true
        modifiedClaim.toOutput.map(_.inspectionDate) shouldBe Right(inspectionDate)

      }
    }

    "submit inspection address" in {
      forAll(InspectionAddressGen.genInspectionAddress) { inspectionAddress =>
        val claim = RejectedGoodsScheduledClaim.empty(exampleEori).submitInspectionAddress(inspectionAddress)

        claim.answers.inspectionAddress shouldBe Some(inspectionAddress)

      }
    }

    "change inspection address" in {
      forAll(completeClaimGen, InspectionAddressGen.genInspectionAddress) { (claim, inspectionAddress) =>
        val modifiedClaim = claim.submitInspectionAddress(inspectionAddress)

        modifiedClaim.hasCompleteAnswers                shouldBe true
        modifiedClaim.toOutput.map(_.inspectionAddress) shouldBe Right(inspectionAddress)
      }
    }

    "submit bankAccountDetails and bankAccountType" in {
      val importDeclarationAllCMAEligible =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val claimEither                     =
        RejectedGoodsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceDutyTypeSetForReimbursement(DutyTypes.custom))
          .flatMap(_.selectAndReplaceTaxCodeSetForDutyType(DutyType.UkDuty, Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(DutyType.UkDuty, TaxCode.A00, BigDecimal("2.00"), BigDecimal("1.00")))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.Business))

      claimEither.getOrFail
    }

    "change bankAccountDetails in a complete claim with all duties CMA eligible" in {
      forAll(completeClaimGen) { claim =>
        val claimEither =
          claim.submitBankAccountDetails(exampleBankAccountDetails)

        claimEither.isRight shouldBe claim.needsBanksAccountDetailsSubmission
      }
    }

    "hasCompleteReimbursementClaims" when {
      "return true if all claim amounts are present" in {
        forAll(completeClaimGen) { claim =>
          claim.hasCompleteReimbursementClaims shouldBe true
        }
      }
    }

    "find next duty type - examples" in {
      val claim = RejectedGoodsScheduledClaim.empty(exampleEori)

      val dutyTypes     = Seq(DutyType.EuDuty, DutyType.Excise)
      val modifiedClaim =
        claim.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes).getOrFail

      modifiedClaim.findNextSelectedDutyAfter(DutyType.Excise) shouldBe None
      modifiedClaim.findNextSelectedDutyAfter(DutyType.EuDuty) shouldBe Some(DutyType.Excise)
    }

    "find next duty type" in {
      val claim = RejectedGoodsScheduledClaim.empty(exampleEori)
      forAll(dutyTypesGen) { dutyTypes =>
        val modifiedClaim =
          claim.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes).getOrFail

        val expected: Seq[(DutyType, DutyType)] = dutyTypes.init.zip(dutyTypes.tail)

        expected.foreach { case (previous, next) =>
          modifiedClaim.findNextSelectedDutyAfter(previous) shouldBe Some(next)
        }

        modifiedClaim.findNextSelectedDutyAfter(dutyTypes.last) shouldBe None
      }
    }

    "find next duty type and tax code pair" in {
      val claim = RejectedGoodsScheduledClaim.empty(exampleEori)
      forAll(dutyTypesWithTaxCodesGen) { (dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]) =>
        whenever(dutyTypesWithTaxCodes.nonEmpty && dutyTypesWithTaxCodes.forall(_._2.nonEmpty)) {

          val dutyTypes: Seq[DutyType] = dutyTypesWithTaxCodes.map(_._1)
          val modifiedClaim            =
            claim
              .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
              .flatMapEach(
                dutyTypesWithTaxCodes,
                j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForDutyType(d._1, d._2)
              )
              .getOrFail

          val expected: Seq[((DutyType, TaxCode), (DutyType, TaxCode))] =
            dutyTypes.init.zip(dutyTypes.tail).flatMap { case (dutyType, nextDutyType) =>
              val taxCodes: Seq[TaxCode] = dutyTypesWithTaxCodes.find(_._1 == dutyType).get._2
              val nextTaxCode: TaxCode   = dutyTypesWithTaxCodes.find(_._1 == nextDutyType).flatMap(_._2.headOption).get

              val edge = Seq(((dutyType, taxCodes.last), (nextDutyType, nextTaxCode)))

              taxCodes.init.zip(taxCodes.tail).map { case (previous, next) =>
                ((dutyType, previous), (dutyType, next))
              } ++ edge
            }

          expected.foreach { case ((previousDutyType, previousTaxCode), (nextDutyType, nextTaxCode)) =>
            modifiedClaim.findNextSelectedTaxCodeAfter(
              previousDutyType,
              previousTaxCode
            ) shouldBe Some((nextDutyType, nextTaxCode))
          }

          val (lastDutyType, lastTaxCode) = {
            val (dt, tcs) = dutyTypesWithTaxCodes.last
            (dt, tcs.last)
          }

          modifiedClaim.findNextSelectedTaxCodeAfter(
            lastDutyType,
            lastTaxCode
          ) shouldBe None
        }
      }
    }

    "validate if any subsidy payment method is in the declaration" in {

      import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport.withSomeSubsidiesPaymentMethod

      val declaration =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
          .withSomeSubsidiesPaymentMethod()

      val claim = RejectedGoodsScheduledClaim
        .empty(
          exampleEori
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
        .getOrFail

      RejectedGoodsScheduledClaim.Checks.declarationsHasNoSubsidyPayments.apply(
        claim
      ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)
    }

  }
}
