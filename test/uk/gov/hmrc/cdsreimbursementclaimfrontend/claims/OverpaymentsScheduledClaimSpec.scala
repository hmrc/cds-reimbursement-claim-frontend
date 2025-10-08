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
import org.scalacheck.ShrinkLowPriority
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimValidationErrors.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator

class OverpaymentsScheduledClaimSpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority
    with Logging {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "OverpaymentsScheduledClaim" should {
    "have an empty instance" in {
      emptyClaim.answers.userEoriNumber                                         shouldBe exampleEori
      emptyClaim.answers.bankAccountDetails                                     shouldBe None
      emptyClaim.answers.bankAccountType                                        shouldBe None
      emptyClaim.answers.basisOfClaim                                           shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.contactDetails                                         shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.eoriNumbersVerification                                shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber) shouldBe None
      emptyClaim.answers.additionalDetails                                      shouldBe None
      emptyClaim.answers.displayDeclaration                                     shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyClaim.answers.correctedAmounts                                       shouldBe None
      emptyClaim.answers.selectedDocumentType                                   shouldBe None
      emptyClaim.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyClaim.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyClaim.answers.dutiesChangeMode                                       shouldBe false
      emptyClaim.getNdrcDetails                                                 shouldBe None
      emptyClaim.getSelectedDutyTypes                                           shouldBe None
      emptyClaim.hasCompleteReimbursementClaims                                 shouldBe false
      emptyClaim.hasCompleteSupportingEvidences                                 shouldBe false
      emptyClaim.hasCompleteAnswers                                             shouldBe false
      emptyClaim.toOutput.isLeft                                                shouldBe true
      emptyClaim.isFinalized                                                    shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeClaimGen) { claim =>
        OverpaymentsScheduledClaim.validator.apply(claim) shouldBe Right(())
        claim.answers.checkYourAnswersChangeMode          shouldBe true
        claim.hasCompleteReimbursementClaims              shouldBe true
        claim.hasCompleteSupportingEvidences              shouldBe true
        claim.hasCompleteAnswers                          shouldBe true
        claim.isFinalized                                 shouldBe false
        claim.getNdrcDetails                              shouldBe defined

        val output = claim.toOutput.fold(e => fail(e.mkString("\n")), identity)

        output.movementReferenceNumber  shouldBe claim.answers.movementReferenceNumber.get
        output.claimantType             shouldBe claim.getClaimantType
        output.basisOfClaim             shouldBe claim.answers.basisOfClaim.get
        output.additionalDetails        shouldBe claim.answers.additionalDetails.get
        output.reimbursementMethod      shouldBe ReimbursementMethod.BankAccountTransfer
        output.reimbursementClaims      shouldBe claim.getReimbursementClaims
        output.supportingEvidences      shouldBe claim.answers.supportingEvidences.map(EvidenceDocument.from)
        output.bankAccountDetails       shouldBe claim.answers.bankAccountDetails
        output.claimantInformation.eori shouldBe claim.answers.userEoriNumber
      }
    }

    "fail produce an output if claim is incomplete" in {
      forAll(buildClaimGen(submitContactDetails = false).map(_.getOrFail)) { claim =>
        claim.hasCompleteAnswers shouldBe false
        claim.isFinalized        shouldBe false
        val result = claim.toOutput
        result.isLeft shouldBe true
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

    "fail finalizing claim when incomplete" in {
      forAll(buildClaimGen(submitContactDetails = false).map(_.getOrFail)) { claim =>
        claim.hasCompleteAnswers shouldBe false
        claim.isFinalized        shouldBe false
        val result = claim.finalizeClaimWith("foo-123-abc")
        result.isLeft shouldBe true
      }
    }

    "accept submission of a new MRN" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, decl) =>
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
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, decl) =>
        val claimEither = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(mrn, decl.withDeclarationId("foo"))

        claimEither shouldBe Left("submitMovementReferenceNumber.wrongDisplayDeclarationMrn")
      }
    }

    "accept change of the MRN" in {
      forAll(completeClaimGen, displayDeclarationGen) { (claim, decl) =>
        val decl2         = decl.withDeclarationId(exampleMrnAsString)
        val modifiedClaim = claim
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, decl2)
          .getOrFail
        modifiedClaim.answers.displayDeclaration     shouldBe Some(decl2)
        modifiedClaim.hasCompleteAnswers             shouldBe false
        modifiedClaim.hasCompleteReimbursementClaims shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences shouldBe false
      }
    }

    "accept change of the MRN when user has XI eori" in {
      forAll(
        completeClaimGen.map(_.submitUserXiEori(UserXiEori(exampleXIEori.value))),
        displayDeclarationGen
      ) { (claim, decl) =>
        val decl2         = decl.withDeclarationId(exampleMrnAsString)
        val modifiedClaim = claim
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, decl2)
          .getOrFail
        modifiedClaim.answers.displayDeclaration      shouldBe Some(decl2)
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
            claim.answers.displayDeclaration.get
          )
          .getOrFail
        modifiedClaim                                shouldBe claim
        modifiedClaim.hasCompleteAnswers             shouldBe true
        modifiedClaim.hasCompleteReimbursementClaims shouldBe true
        modifiedClaim.hasCompleteSupportingEvidences shouldBe true
      }
    }

    "accept submission of a new ACC14 data" in {
      forAll(displayDeclarationGen) { acc14 =>
        val claim = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(
            exampleMrn,
            acc14.withDeclarationId(exampleMrnAsString)
          )
          .getOrFail

        claim.answers.movementReferenceNumber.contains(exampleMrn)                             shouldBe true
        claim.answers.displayDeclaration.contains(acc14.withDeclarationId(exampleMrnAsString)) shouldBe true
        claim.hasCompleteAnswers                                                               shouldBe false
        claim.hasCompleteReimbursementClaims                                                   shouldBe false
        claim.hasCompleteSupportingEvidences                                                   shouldBe false
      }
    }

    "accept change of the ACC14 data" in {
      forAll(completeClaimGen) { claim =>
        val modifiedClaim =
          claim
            .submitMovementReferenceNumberAndDeclaration(
              exampleMrn,
              exampleDisplayDeclaration
            )
            .getOrFail
        modifiedClaim.answers.movementReferenceNumber shouldBe Some(exampleMrn)
        modifiedClaim.answers.displayDeclaration      shouldBe Some(exampleDisplayDeclaration)
        modifiedClaim.answers.correctedAmounts        shouldBe None
        modifiedClaim.hasCompleteAnswers              shouldBe false
        modifiedClaim.hasCompleteReimbursementClaims  shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences  shouldBe false
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val claim              =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe true
      claim.getClaimantType                          shouldBe ClaimantType.User
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "needs XI eori submission if user's eori not matching those of ACC14 and ACC14 contains XI eori" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val claim              =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
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
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori, consigneeEORI = None)
      val claim              =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's eori is matching that of consignee" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleEori))
      val claim              =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Consignee
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is missing" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleXIEori, consigneeEORI = None)
      val claim              =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is present" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleXIEori, consigneeEORI = Some(anotherExampleEori))
      val claim              =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of consignee" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val claim              =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
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
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val claimEither        =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val claimEither        =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "fail if submitted declarant EORI is not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val claimEither        =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val claimEither        =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val claim =
          OverpaymentsScheduledClaim
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
        val claim = OverpaymentsScheduledClaim.empty(exampleEori).submitContactAddress(contactAddress)

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
      forAll(Gen.oneOf(BasisOfOverpaymentClaim.values)) { basisOfClaim =>
        val claim = OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitBasisOfClaim(basisOfClaim)
        claim.answers.basisOfClaim shouldBe Some(basisOfClaim)
      }
    }

    "change basis of claim" in {
      forAll(
        completeClaimGen,
        Gen.oneOf(
          BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.IncorrectExciseValue - BasisOfOverpaymentClaim.IncorrectEoriAndDan
        )
      ) { (claim, basisOfClaim) =>
        val modifiedClaim = claim.submitBasisOfClaim(basisOfClaim)

        modifiedClaim.hasCompleteAnswers           shouldBe true
        modifiedClaim.toOutput.map(_.basisOfClaim) shouldBe Right(basisOfClaim)
      }
    }

    "submit additional details" in {
      val claim = OverpaymentsScheduledClaim
        .empty(exampleEori)
        .submitAdditionalDetails("foo bar")

      claim.answers.additionalDetails shouldBe Some("foo bar")
    }

    "change additional details" in {
      forAll(completeClaimGen, Gen.asciiPrintableStr) { (claim, additionalDetails) =>
        val modifiedClaim = claim.submitAdditionalDetails(additionalDetails)

        modifiedClaim.hasCompleteAnswers                shouldBe true
        modifiedClaim.toOutput.map(_.additionalDetails) shouldBe Right(additionalDetails)
      }
    }

    "select duty types for reimbursement when none yet selected" in {
      forAll(dutyTypesGen) { (dutyTypes: Seq[DutyType]) =>
        val claim = OverpaymentsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .getOrFail

        claim.getSelectedDutyTypes shouldBe Some(dutyTypes)
      }
    }

    "replace duty types for reimbursement" in {
      forAll(dutyTypesGen, dutyTypesGen) { (dutyTypes1: Seq[DutyType], dutyTypes2: Seq[DutyType]) =>
        val claim = OverpaymentsScheduledClaim
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
        val claim     = OverpaymentsScheduledClaim
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
          val claim = OverpaymentsScheduledClaim
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
        val result    = OverpaymentsScheduledClaim
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
        val result    = OverpaymentsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(DutyTypes.all.toSet.diff(dutyTypes.toSet).toSeq)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            claim => (tc: (DutyType, Seq[TaxCode])) => claim.selectAndReplaceTaxCodeSetForDutyType(tc._1, tc._2)
          )

        result shouldBe Left("selectTaxCodeSetForReimbursement.dutyTypeNotSelectedBefore")
      }
    }

    "return left if empty duty types selection" in {
      val dutyTypes: Seq[DutyType] = Seq.empty
      val result                   = OverpaymentsScheduledClaim
        .empty(exampleEori)
        .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)

      result shouldBe Left("selectAndReplaceDutyTypeSetForReimbursement.emptySelection")
    }

    "return left if empty tax type selection" in {
      forAll(dutyTypesGen) { (dutyTypes: Seq[DutyType]) =>
        val taxTypes: Seq[TaxCode] = List.empty
        val result                 = OverpaymentsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypes,
            claim => (dt: DutyType) => claim.selectAndReplaceTaxCodeSetForDutyType(dt, taxTypes)
          )

        result shouldBe Left("selectTaxCodeSetForReimbursement.emptySelection")
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

        val claim = OverpaymentsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            (j: OverpaymentsScheduledClaim) =>
              { case (dutyType: DutyType, taxCodeSeq: Seq[TaxCode]) =>
                j.selectAndReplaceTaxCodeSetForDutyType(dutyType, taxCodeSeq)
              }: (((DutyType, Seq[TaxCode])) => Either[String, OverpaymentsScheduledClaim])
          )
          .flatMapEach(
            taxCodesWithAmounts,
            (j: OverpaymentsScheduledClaim) =>
              { case (dutyType: DutyType, taxCode: TaxCode, paidAmount: BigDecimal, correctAmount: BigDecimal) =>
                j.submitCorrectAmount(dutyType, taxCode, paidAmount, correctAmount)
              }: (((DutyType, TaxCode, BigDecimal, BigDecimal)) => Either[String, OverpaymentsScheduledClaim])
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

        val claim = OverpaymentsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            (j: OverpaymentsScheduledClaim) =>
              { case (dutyType: DutyType, taxCodeSeq: Seq[TaxCode]) =>
                j.selectAndReplaceTaxCodeSetForDutyType(dutyType, taxCodeSeq)
              }: (((DutyType, Seq[TaxCode])) => Either[String, OverpaymentsScheduledClaim])
          )
          .flatMapEach(
            taxCodesWithAmounts,
            (j: OverpaymentsScheduledClaim) =>
              { case (dutyType: DutyType, taxCode: TaxCode, paidAmount: BigDecimal, correctAmount: BigDecimal) =>
                j.submitClaimAmount(dutyType, taxCode, paidAmount, paidAmount - correctAmount)
              }: (((DutyType, TaxCode, BigDecimal, BigDecimal)) => Either[String, OverpaymentsScheduledClaim])
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

    "return false if at least one of the claimed tax code do not have a value specified" in {
      forAll(dutyTypesWithTaxCodesWithClaimAmountsGen) { data =>
        val dutyTypes: Seq[DutyType]                                              = data.map(_._1)
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                  = data.map { case (dt, tcs) => dt -> tcs.map(_._1) }
        val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)] = data.flatMap { case (dt, tca) =>
          tca.map { case (tc, pa, ca) => (dt, tc, pa, ca) }
        }

        val claim = OverpaymentsScheduledClaim
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForDutyType(d._1, d._2)
          )
          .flatMapEach(
            taxCodesWithAmounts.dropRight(1),
            j => (d: (DutyType, TaxCode, BigDecimal, BigDecimal)) => j.submitCorrectAmount(d._1, d._2, d._3, d._4)
          )
          .getOrFail

        claim.hasCompleteReimbursementClaims shouldBe false
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

        val result = OverpaymentsScheduledClaim
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

        val result = OverpaymentsScheduledClaim
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

    "get available document types and claim types" in {
      val displayDeclaration     = exampleDisplayDeclaration
      val availableDocumentTypes = UploadDocumentType.overpaymentsScheduledDocumentTypes

      val availableClaimTypes =
        BasisOfOverpaymentClaim
          .excludeNorthernIrelandClaims(false, Some(displayDeclaration), isOtherEnabled = true)

      val claim = OverpaymentsScheduledClaim
        .empty(
          exampleEori,
          features = Some(
            OverpaymentsScheduledClaim
              .Features(
                shouldAllowOtherBasisOfClaim = true
              )
          )
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .getOrFail

      claim.getDocumentTypesIfRequired shouldBe Some(availableDocumentTypes)

      claim.getAvailableClaimTypes shouldBe availableClaimTypes

      for document <- availableDocumentTypes do {
        val result = claim.submitDocumentTypeSelection(document)

        result.getSelectedDocumentType shouldBe Some(document)
      }
    }

    "get available claim types except for Other when OtherBasisOfClaim feature is disabled" in {
      val displayDeclaration = exampleDisplayDeclaration

      val claim = OverpaymentsScheduledClaim
        .empty(
          exampleEori,
          features = Some(
            OverpaymentsScheduledClaim
              .Features(
                shouldAllowOtherBasisOfClaim = false
              )
          )
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .getOrFail

      claim.getAvailableClaimTypes.contains(BasisOfOverpaymentClaim.Miscellaneous) shouldBe false
    }

    "reject submit invalid amount for valid selected tax code" in {
      forAll(dutyTypesWithTaxCodesWithClaimAmountsGen) { data =>
        val dutyTypes: Seq[DutyType]                                              = data.map(_._1)
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                  = data.map { case (dt, tcs) => dt -> tcs.map(_._1) }
        val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)] = data.flatMap { case (dt, tca) =>
          tca.map { case (tc, pa, ca) => (dt, tc, pa, ca) }
        }

        val result = OverpaymentsScheduledClaim
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

    "computeBankDetails" should {
      "return consigneeBankDetails when payeeType is consignee" in {
        val declaration = buildDisplayDeclaration(
          dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)),
          consigneeBankDetails = Some(exampleConsigneeBankAccountDetails),
          declarantBankDetails = Some(exampleDeclarantBankAccountDetails)
        )

        val claimEither =
          OverpaymentsScheduledClaim
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
            .flatMap(_.selectAndReplaceDutyTypeSetForReimbursement(DutyTypes.custom))
            .flatMap(_.selectAndReplaceTaxCodeSetForDutyType(DutyType.UkDuty, Seq(TaxCode.A00)))
            .flatMap(_.submitCorrectAmount(DutyType.UkDuty, TaxCode.A00, BigDecimal("2.00"), BigDecimal("1.00")))
            .flatMap(_.submitPayeeType(PayeeType.Consignee))

        claimEither.isRight                                shouldBe true
        claimEither.toOption.get.computeBankAccountDetails shouldBe Some(exampleConsigneeBankAccountDetails)
      }

      "return declarantBankDetails when payeeType is declarant" in {
        val declaration = buildDisplayDeclaration(
          dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)),
          consigneeBankDetails = Some(exampleConsigneeBankAccountDetails),
          declarantBankDetails = Some(exampleDeclarantBankAccountDetails)
        )

        val claimEither =
          OverpaymentsScheduledClaim
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
            .flatMap(_.selectAndReplaceDutyTypeSetForReimbursement(DutyTypes.custom))
            .flatMap(_.selectAndReplaceTaxCodeSetForDutyType(DutyType.UkDuty, Seq(TaxCode.A00)))
            .flatMap(_.submitCorrectAmount(DutyType.UkDuty, TaxCode.A00, BigDecimal("2.00"), BigDecimal("1.00")))
            .flatMap(_.submitPayeeType(PayeeType.Declarant))

        claimEither.isRight                                shouldBe true
        claimEither.toOption.get.computeBankAccountDetails shouldBe Some(exampleDeclarantBankAccountDetails)
      }
    }

    "submit bankAccountDetails and bankAccountType" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val claimEither                      =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
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
      val claim = OverpaymentsScheduledClaim.empty(exampleEori)

      val dutyTypes     = Seq(DutyType.EuDuty, DutyType.Excise)
      val modifiedClaim =
        claim.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes).getOrFail

      modifiedClaim.findNextSelectedDutyAfter(DutyType.Excise) shouldBe None
      modifiedClaim.findNextSelectedDutyAfter(DutyType.EuDuty) shouldBe Some(DutyType.Excise)
    }

    "find next duty type" in {
      val claim = OverpaymentsScheduledClaim.empty(exampleEori)
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
      val claim = OverpaymentsScheduledClaim.empty(exampleEori)
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

    "get ndrc details and amounts for a claim" in {
      val nextDetailsList: List[NdrcDetails] =
        exampleDisplayDeclaration.getNdrcDetailsList.get

      val taxCodesWithTypes: Map[DutyType, List[TaxCode]] =
        nextDetailsList
          .map { nextDetails =>
            val taxCode  = TaxCode(nextDetails.taxType)
            val dutyType = DutyTypes.all.find(_.taxCodes.contains(taxCode)).get
            logger.warn(s"taxcode value: ${taxCode.value}, dutyType value: ${dutyType.repr}")
            (taxCode, dutyType)
          }
          .groupBy(_._2)
          .view
          .mapValues(_.map(_._1))
          .toMap

      val dutyTypes: List[DutyType] =
        taxCodesWithTypes.keys.toList

      var claim =
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
          .flatMap(_.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes))
          .getOrFail

      var previousDuty: Option[DutyType] = None

      var previousTaxCode: Option[TaxCode] = None

      for (dutyType, taxCodes) <- taxCodesWithTypes.toSeq.sortBy(_._1) do {
        claim.findNextDutyToSelectDuties shouldBe Some(dutyType)

        previousDuty.foreach { pdt =>
          claim.findNextSelectedDutyAfter(pdt) shouldBe Some(dutyType)
        }

        claim = claim
          .selectAndReplaceTaxCodeSetForDutyType(dutyType, taxCodes)
          .getOrFail

        previousTaxCode = None

        for taxCode <- taxCodes.sorted do {
          val ndrcDetails = nextDetailsList.find(d => d.taxType === taxCode.value).get
          val amount      = BigDecimal(ndrcDetails.amount)

          claim.getNdrcDetailsFor(taxCode) shouldBe Some(ndrcDetails)

          claim.getReimbursementFor(dutyType, taxCode) shouldBe None

          claim = claim
            .submitCorrectAmount(dutyType, taxCode, amount, ZERO)
            .getOrFail

          claim.getReimbursementFor(dutyType, taxCode) shouldBe Some(AmountPaidWithCorrect(amount, ZERO))

          previousTaxCode.foreach { ptc =>
            claim.findNextSelectedTaxCodeAfter(dutyType, ptc) shouldBe Some((dutyType, taxCode))
          }

          previousTaxCode = Some(taxCode)
        }

        previousDuty = Some(dutyType)
      }

      claim.findNextDutyToSelectDuties shouldBe None

      previousDuty.foreach { pdt =>
        claim.findNextSelectedDutyAfter(pdt) shouldBe None

        previousTaxCode.foreach { ptc =>
          claim.findNextSelectedTaxCodeAfter(pdt, ptc) shouldBe None
        }
      }
    }

    "get and update selected duties" in {
      forAll(dutyTypesWithTaxCodesGen) { (dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]) =>
        whenever(dutyTypesWithTaxCodes.nonEmpty && dutyTypesWithTaxCodes.forall(_._2.nonEmpty)) {

          val dutyTypes: Seq[DutyType] = dutyTypesWithTaxCodes.map(_._1)

          val claim = OverpaymentsScheduledClaim
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .getOrFail

          claim.getSelectedDuties shouldBe Map.empty

          val modifiedClaim = claim
            .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
            .flatMapEach(
              dutyTypesWithTaxCodes,
              j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForDutyType(d._1, d._2)
            )
            .getOrFail

          val selectedDuties = dutyTypesWithTaxCodes.toMap

          modifiedClaim.getSelectedDuties shouldBe selectedDuties

          val updateDuties = modifiedClaim
            .withDutiesChangeMode(true)

          updateDuties.answers.dutiesChangeMode shouldBe true

        }
      }
    }

    "receiveUploadedFiles fails when nonce is not matching the claim nonce" in {
      val claim  = OverpaymentsScheduledClaim.empty(exampleEori)
      val result = claim.receiveUploadedFiles(Some(UploadDocumentType.ProofOfAuthority), Nonce.random, Seq.empty)
      result shouldBe Left("receiveUploadedFiles.invalidNonce")
    }

    "receiveUploadedFiles fills in missing documentType" in {
      val claim        = OverpaymentsScheduledClaim.empty(exampleEori)
      val uploadedFile = buildUploadDocument("foo").copy(cargo = None)
      val result       =
        claim
          .receiveUploadedFiles(Some(UploadDocumentType.ProofOfAuthority), claim.answers.nonce, Seq(uploadedFile))
          .getOrFail
      result.answers.supportingEvidences.head shouldBe uploadedFile.copy(cargo =
        Some(UploadDocumentType.ProofOfAuthority)
      )
    }
    "tryBuildFrom should return upload type other" in {
      val specialClaimGen: Gen[OverpaymentsScheduledClaim] = buildClaimGenWithoutSupportingEvidence()
      forAll(specialClaimGen) { (claim: OverpaymentsScheduledClaim) =>
        claim.answers.supportingEvidences.foreach(uploadedFile =>
          uploadedFile.cargo.get shouldBe UploadDocumentType.Other
        )
      }
    }
    "receiveScheduledDocument fails when nonce is not matching the claim nonce" in {
      val claim  = OverpaymentsScheduledClaim.empty(exampleEori)
      val result = claim.receiveScheduledDocument(Nonce.random, buildUploadDocument("foo"))
      result shouldBe Left("receiveScheduledDocument.invalidNonce")
    }
    "removeScheduledDocument" in {
      val claim         = OverpaymentsScheduledClaim.empty(exampleEori)
      val nonce         = claim.answers.nonce
      val uploadedFile  = buildUploadDocument("foo").copy(cargo = None)
      val modifiedClaim = claim.receiveScheduledDocument(nonce, uploadedFile)

      modifiedClaim.getOrFail.answers.scheduledDocument shouldBe Some(uploadedFile)
      val result =
        claim.removeScheduledDocument
      result.answers.scheduledDocument shouldBe None
    }

    "check if claim instance is equal to itself" in {
      val claim      = OverpaymentsScheduledClaim
        .empty(exampleEori)
      val trueResult = claim
      claim.equals(trueResult) shouldBe true

      val falseResult = OverpaymentsScheduledClaim
        .empty(exampleEori)
      claim.equals(falseResult) shouldBe false
    }

    "return claim hash code" in {
      val claim = OverpaymentsScheduledClaim
        .empty(exampleEori)

      claim.answers.hashCode() shouldBe claim.hashCode()
    }

    "serialises and deserialises json" in {
      forAll(completeClaimGen) { data =>
        val claim: OverpaymentsScheduledClaim = data

        val json: JsValue = Json.toJson(claim)
        val result        = Json.parse(json.toString()).asOpt[OverpaymentsScheduledClaim]

        result shouldBe defined
        claim  shouldBe result.get
      }
    }

    "check for invalid claim" in {
      forAll(completeClaimGen) { data =>
        val invalidClaim: OverpaymentsScheduledClaim = data.removeScheduledDocument

        invalidClaim.toOutput shouldBe Left(
          List("missingScheduledDocument")
        )
      }
    }

    "validate if any subsidy payment method is in the declaration" in {

      import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport.withSomeSubsidiesPaymentMethod

      val declaration =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
          .withSomeSubsidiesPaymentMethod()

      val claim = OverpaymentsScheduledClaim
        .empty(
          exampleEori
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
        .getOrFail

      OverpaymentsScheduledClaim.Checks.declarationsHasNoSubsidyPayments.apply(
        claim
      ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)

    }
  }
}
