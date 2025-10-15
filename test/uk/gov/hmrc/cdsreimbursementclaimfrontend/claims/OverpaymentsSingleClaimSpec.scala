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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimValidationErrors.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator

import java.util.Locale

class OverpaymentsSingleClaimSpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "OverpaymentsSingleClaim" should {
    "have an empty instance" in {
      emptyClaim.answers.userEoriNumber                                         shouldBe exampleEori
      emptyClaim.answers.bankAccountDetails                                     shouldBe None
      emptyClaim.answers.bankAccountType                                        shouldBe None
      emptyClaim.answers.basisOfClaim                                           shouldBe None
      emptyClaim.answers.duplicateDeclaration                                   shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.contactDetails                                         shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.eoriNumbersVerification                                shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber) shouldBe None
      emptyClaim.answers.additionalDetails                                      shouldBe None
      emptyClaim.answers.importDeclaration                                      shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyClaim.answers.correctedAmounts                                       shouldBe None
      emptyClaim.answers.reimbursementMethod                                    shouldBe None
      emptyClaim.answers.selectedDocumentType                                   shouldBe None
      emptyClaim.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyClaim.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyClaim.answers.dutiesChangeMode                                       shouldBe false
      emptyClaim.getNdrcDetails                                                 shouldBe None
      emptyClaim.getSelectedDuties                                              shouldBe None
      emptyClaim.isAllSelectedDutiesAreCMAEligible                              shouldBe false
      emptyClaim.hasCompleteReimbursementClaims                                 shouldBe false
      emptyClaim.hasCompleteSupportingEvidences                                 shouldBe false
      emptyClaim.hasCompleteAnswers                                             shouldBe false
      emptyClaim.toOutput.isLeft                                                shouldBe true
      emptyClaim.isFinalized                                                    shouldBe false
      emptyClaim.containsUnsupportedTaxCode                                     shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeClaimGen) { claim =>
        OverpaymentsSingleClaim.validator.apply(claim) shouldBe Right(())
        claim.answers.checkYourAnswersChangeMode       shouldBe true
        claim.hasCompleteReimbursementClaims           shouldBe true
        claim.hasCompleteSupportingEvidences           shouldBe true
        claim.hasCompleteAnswers                       shouldBe true
        claim.isFinalized                              shouldBe false
        claim.containsUnsupportedTaxCode               shouldBe false

        claim.getTotalReimbursementAmount shouldBe (
          claim.getEUDutyReimbursementTotal.getOrElse(ZERO) +
            claim.getUKDutyReimbursementTotal.getOrElse(ZERO) +
            claim.getExciseDutyReimbursementTotal.getOrElse(ZERO)
        )

        val output = claim.toOutput.getOrElse(fail("Claim output not defined."))

        output.movementReferenceNumber  shouldBe claim.answers.movementReferenceNumber.get
        output.claimantType             shouldBe claim.getClaimantType
        output.basisOfClaim             shouldBe claim.answers.basisOfClaim.get
        output.additionalDetails        shouldBe claim.answers.additionalDetails.get
        output.reimbursementMethod      shouldBe claim.answers.reimbursementMethod
          .getOrElse(ReimbursementMethod.BankAccountTransfer)
        output.reimbursements           shouldBe claim.getReimbursements
        output.supportingEvidences      shouldBe claim.answers.supportingEvidences.map(EvidenceDocument.from)
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

    "fail to finalize invalid claim" in {
      completeClaimGen.sample.get
        .removeBankAccountDetails()
        .finalizeClaimWith("foo")
        .isLeft shouldBe true
    }

    "have working equals method" in {
      val claim = completeClaimGen.sample.get
      claim.equals(completeClaimGen.sample.get) shouldBe false
      claim.equals(claim)                       shouldBe true
      claim.equals("foo")                       shouldBe false
      claim.hashCode()                          shouldBe claim.answers.hashCode
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
        OverpaymentsSingleClaim
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
        OverpaymentsSingleClaim
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

    "needs XI eori submission if user's eori not matching those of a duplicate ACC14, and duplicate ACC14 contains XI eori" in {
      val duplicateImportDeclaration =
        buildImportDeclaration(
          id = anotherExampleMrn.value,
          declarantEORI = anotherExampleEori,
          consigneeEORI = Some(exampleXIEori)
        )

      val claim =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(
            exampleMrn,
            exampleImportDeclaration
              .withDeclarationId(exampleMrn.value)
              .withDeclarantEori(exampleEori)
          )
          .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
          .flatMap(
            _.submitDuplicateMovementReferenceNumberAndDeclaration(
              anotherExampleMrn,
              duplicateImportDeclaration
            )
          )
          .getOrFail

      claim.userHasGBEoriMatchingDeclaration         shouldBe true
      claim.userHasXIEoriMatchingDeclaration         shouldBe false
      claim.needsUserXiEoriSubmission                shouldBe false
      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false

      claim.userHasGBEoriMatchingDuplicateDeclaration        shouldBe false
      claim.userHasXIEoriMatchingDuplicateDeclaration        shouldBe false
      claim.needsUserXiEoriSubmissionForDuplicateDeclaration shouldBe true

      val claim2 = claim.submitUserXiEori(UserXiEori(exampleXIEori.value))

      claim2.userHasGBEoriMatchingDeclaration         shouldBe true
      claim2.userHasXIEoriMatchingDeclaration         shouldBe false
      claim2.needsUserXiEoriSubmission                shouldBe false
      claim2.needsDeclarantAndConsigneeEoriSubmission shouldBe false

      claim2.userHasGBEoriMatchingDuplicateDeclaration        shouldBe false
      claim2.userHasXIEoriMatchingDuplicateDeclaration        shouldBe true
      claim2.needsUserXiEoriSubmissionForDuplicateDeclaration shouldBe false
    }

    "does not need declarant and consignee submission if user's eori is matching that of declarant" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleEori, consigneeEORI = None)
      val claim             =
        OverpaymentsSingleClaim
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
        OverpaymentsSingleClaim
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
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is present" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleXIEori, consigneeEORI = Some(anotherExampleEori))
      val claim             =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of consignee" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val claim             =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(Locale.ENGLISH))))
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
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori)
      val claimEither       =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "submit declarant eori number" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori)
      val claimEither       =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      claimEither.getOrFail.answers.eoriNumbersVerification.get.declarantEoriNumber shouldBe Some(anotherExampleEori)
    }

    "fail if submitted declarant EORI is not needed" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleEori)
      val claimEither       =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori)
      val claimEither       =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val claim =
          OverpaymentsSingleClaim
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
        val claim = OverpaymentsSingleClaim.empty(exampleEori).submitContactAddress(contactAddress)

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
        val claim = OverpaymentsSingleClaim.empty(exampleEori).submitBasisOfClaim(basisOfClaim)
        claim.answers.basisOfClaim shouldBe Some(basisOfClaim)
      }
    }

    "change basis of claim" in {
      forAll(
        completeClaimGenWithoutDuplicateEntryAndIncorrectExciseValue,
        Gen.oneOf(
          BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.DuplicateEntry - BasisOfOverpaymentClaim.IncorrectExciseValue - BasisOfOverpaymentClaim.IncorrectEoriAndDan
        )
      ) { (claim, basisOfClaim) =>
        val modifiedClaim = claim.submitBasisOfClaim(basisOfClaim)

        modifiedClaim.hasCompleteAnswers           shouldBe true
        modifiedClaim.toOutput.map(_.basisOfClaim) shouldBe Right(basisOfClaim)
      }
    }

    "change basis of claim if duplicate entry" in {
      forAll(completeClaimGenWithoutDuplicateEntryAndIncorrectExciseValue) { claim =>
        val modifiedClaim = claim.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry)

        modifiedClaim.hasCompleteAnswers           shouldBe false
        modifiedClaim.toOutput.map(_.basisOfClaim) shouldBe Left(
          DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_BE_DEFINED :: DUPLICATE_DISPLAY_DECLARATION_MUST_BE_DEFINED :: Nil
        )
      }
    }

    "change basis of claim if incorrect excise entry" in {
      forAll(completeClaimGenWithoutDuplicateEntryAndIncorrectExciseValue) { claim =>
        val modifiedClaim = claim.submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectExciseValue)

        modifiedClaim.hasCompleteAnswers shouldBe modifiedClaim.hasCompleteReimbursementClaims
      }
    }

    "change basis of claim if IncorrectEoriAndDan" in {
      val basisOfClaim = BasisOfOverpaymentClaim.IncorrectEoriAndDan
      val claimEither  = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitBasisOfClaim(basisOfClaim)
        .submitNewDan(exampleDan)
        .submitNewEori(exampleEori)

      val modifiedClaim = claimEither.submitBasisOfClaim(basisOfClaim)

      modifiedClaim.answers.basisOfClaim shouldBe Some(basisOfClaim)
      modifiedClaim.answers.newEori      shouldBe Some(exampleEori)
      modifiedClaim.answers.newDan       shouldBe Some(exampleDan)
    }

    "submit duplicate mrn and declaration" in {
      val claimEither = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleImportDeclaration)
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
        .flatMap(
          _.submitDuplicateMovementReferenceNumberAndDeclaration(
            anotherExampleMrn,
            exampleImportDeclaration.withDeclarationId(anotherExampleMrn.value)
          )
        )

      claimEither.isRight shouldBe true
    }

    "remove duplicated declaration when basis of claim changes" in {
      val claimEither = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleImportDeclaration)
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
        .flatMap(
          _.submitDuplicateMovementReferenceNumberAndDeclaration(
            anotherExampleMrn,
            exampleImportDeclaration.withDeclarationId(anotherExampleMrn.value)
          )
        )
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.EndUseRelief))

      claimEither.isRight                                shouldBe true
      claimEither.getOrFail.answers.duplicateDeclaration shouldBe None
    }

    "reject duplicate mrn and declaration if same mrn as main" in {
      val claimEither = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleImportDeclaration)
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
        .flatMap(
          _.submitDuplicateMovementReferenceNumberAndDeclaration(
            exampleMrn,
            exampleImportDeclaration
          )
        )

      claimEither.isRight shouldBe false
    }

    "reject duplicate mrn and declaration if same declaration as main" in {
      val claimEither = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleImportDeclaration)
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
        .flatMap(
          _.submitDuplicateMovementReferenceNumberAndDeclaration(
            anotherExampleMrn,
            exampleImportDeclaration
          )
        )

      claimEither.isRight shouldBe false
    }

    "change duplicate mrn and declaration" in {
      forAll(completeClaimGenWithDuplicateEntry) { claim =>
        val duplicateMrn        = IdGen.genMRN.sample.get
        val modifiedClaimEither =
          claim
            .submitDuplicateMovementReferenceNumberAndDeclaration(
              duplicateMrn,
              exampleImportDeclaration.withDeclarationId(duplicateMrn.value)
            )

        modifiedClaimEither.isRight shouldBe claim.needsDuplicateMrnAndDeclaration
      }
    }

    "submit additional details" in {
      val claim = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitAdditionalDetails("foo bar")

      claim.answers.additionalDetails shouldBe Some("foo bar")
    }

    "change details of rejected goods" in {
      forAll(completeClaimGen, Gen.asciiPrintableStr) { (claim, additionalDetails) =>
        val modifiedClaim = claim.submitAdditionalDetails(additionalDetails)

        modifiedClaim.hasCompleteAnswers                shouldBe true
        modifiedClaim.toOutput.map(_.additionalDetails) shouldBe Right(additionalDetails)
      }
    }

    "select valid tax codes for reimbursement when none yet selected" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00, TaxCode.A90)))

      claimEither.isRight                    shouldBe true
      claimEither.getOrFail.getAvailableDuties should contain theSameElementsAs Seq(
        (TaxCode.A00, false),
        (TaxCode.A90, false)
      )
    }

    "return left when selecting valid tax codes for reimbursement without a display declaration" in {
      val claimEither = OverpaymentsSingleClaim
        .empty(exampleEori)
        .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00, TaxCode.A90))

      claimEither shouldBe Left("selectTaxCodeSetForReimbursement.missingImportDeclaration")
    }

    "return left when selecting empty list of tax codes for reimbursement" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq.empty))

      claimEither shouldBe Left("selectTaxCodeSetForReimbursement.emptySelection")
    }

    "select valid tax codes for reimbursement when none yet selected with excise code in declaration" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.NI407, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectExciseValue))
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.NI407)))

      claimEither.isRight                    shouldBe true
      claimEither.getOrFail.getAvailableDuties should contain theSameElementsAs Seq((TaxCode.NI407, false))
    }

    "return all available claim types when excise code in declaration" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.NI407, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(
          exampleEori,
          features = Some(
            OverpaymentsSingleClaim
              .Features(
                shouldAllowOtherBasisOfClaim = true
              )
          )
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)

      claimEither.isRight                        shouldBe true
      claimEither.getOrFail.getAvailableClaimTypes should contain theSameElementsAs BasisOfOverpaymentClaim.values
    }

    "return all available claim types except for Other when OtherBasisOfClaim feature is disabled" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.NI407, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(
          exampleEori,
          features = Some(
            OverpaymentsSingleClaim
              .Features(
                shouldAllowOtherBasisOfClaim = false
              )
          )
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)

      claimEither.getOrFail.getAvailableClaimTypes.contains(BasisOfOverpaymentClaim.Miscellaneous) shouldBe false
    }

    "return all available claim types except IncorrectExciseValue when no excise code in declaration" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(
          exampleEori,
          features = Some(
            OverpaymentsSingleClaim
              .Features(
                shouldAllowOtherBasisOfClaim = true
              )
          )
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)

      claimEither.isRight                        shouldBe true
      claimEither.getOrFail.getAvailableClaimTypes should contain theSameElementsAs (BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.IncorrectExciseValue)
    }

    "replace valid tax codes for reimbursement" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))

      claimEither.getOrFail.getSelectedDuties shouldBe Some(Seq(TaxCode.A00))

      val claimEither2 =
        claimEither.flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A90, TaxCode.A20)))

      claimEither2.getOrFail.getSelectedDuties shouldBe Some(
        Seq(TaxCode.A90, TaxCode.A20)
      )

    }

    "replace valid tax codes for reimbursement with non-CMA duty addition when CurrentMonthAdjustment" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A20)))
        .flatMap(_.submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment))

      claimEither.getOrFail.getSelectedDuties           shouldBe Some(Seq(TaxCode.A20))
      claimEither.getOrFail.answers.reimbursementMethod shouldBe Some(ReimbursementMethod.CurrentMonthAdjustment)

      val claimEither2 =
        claimEither.flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A20, TaxCode.A00)))

      claimEither2.getOrFail.getSelectedDuties           shouldBe Some(
        Seq(TaxCode.A20, TaxCode.A00)
      )
      claimEither2.getOrFail.answers.reimbursementMethod shouldBe None
    }

    "replace valid tax codes for reimbursement with non-CMA duty addition when BankAccountTransfer" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A20)))
        .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))

      claimEither.getOrFail.getSelectedDuties           shouldBe Some(Seq(TaxCode.A20))
      claimEither.getOrFail.answers.reimbursementMethod shouldBe Some(ReimbursementMethod.BankAccountTransfer)

      val claimEither2 =
        claimEither.flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A20, TaxCode.A00)))

      claimEither2.getOrFail.getSelectedDuties           shouldBe Some(
        Seq(TaxCode.A20, TaxCode.A00)
      )
      claimEither2.getOrFail.answers.reimbursementMethod shouldBe None
    }

    "select invalid tax codes for reimbursement" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("1.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A80)))

      claimEither.isRight shouldBe false
    }

    "change tax code for reimbursement with the same set" in {
      forAll(completeClaimGen) { claim =>
        val modifiedClaimEither = claim.selectAndReplaceTaxCodeSetForReimbursement(claim.getSelectedDuties.get)

        val result = modifiedClaimEither.getOrFail
        result.hasCompleteAnswers shouldBe true
      }
    }

    "change tax code for reimbursement with a new valid set" in {
      forAll(completeClaimGen) { claim =>
        val taxCodeSet                  = claim.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val newTaxCodeSet: Seq[TaxCode] = taxCodeSet.take(2).toSeq

        val modifiedClaimEither = claim.selectAndReplaceTaxCodeSetForReimbursement(newTaxCodeSet)

        val result = modifiedClaimEither.getOrFail
        result.getSelectedDuties.get shouldBe newTaxCodeSet
      }
    }

    "change tax code for reimbursement with a new invalid set" in {
      forAll(completeClaimGen) { claim =>
        val invalidTaxCodeSet   = TaxCodes.all.take(6)
        val modifiedClaimEither = claim.selectAndReplaceTaxCodeSetForReimbursement(invalidTaxCodeSet)
        modifiedClaimEither shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
      }
    }

    "submit valid correct amount for selected tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("5.00")))

      claimEither.isRight shouldBe true
    }

    "submit valid claim amount for selected tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("6.66")))

      claimEither.isRight                               shouldBe true
      claimEither.getOrFail.getTotalReimbursementAmount shouldBe BigDecimal("6.66")
    }

    "submit valid correct amount for wrong tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A80, BigDecimal("5.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "submit valid claim amount for wrong tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(TaxCode.A80, BigDecimal("6.66")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "return left when submitting valid correct amount with missing display declaration" in {
      val claimEither = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitCorrectAmount(TaxCode.A80, BigDecimal("5.00"))

      claimEither shouldBe Left("submitCorrectAmount.missingImportDeclaration")
    }

    "return left when submitting valid claim amount with missing display declaration" in {
      val claimEither = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitClaimAmount(TaxCode.A80, BigDecimal("6.66"))

      claimEither shouldBe Left("submitCorrectAmount.missingImportDeclaration")
    }

    "return left when submitting valid correct amount for with no tax code selected" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("5.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotSelectedYet")
    }

    "return left when submitting valid claim amount for with no tax code selected" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("6.66")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotSelectedYet")
    }

    "submit invalid correct amount for selected tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))

      val claimEitherTestZero     =
        declaration.flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("10.00")))
      val claimEitherTestNegative =
        declaration.flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("-10.00")))
      val claimEitherTestGreater  =
        declaration.flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("20.00")))

      claimEitherTestZero     shouldBe Left("submitCorrectAmount.invalidAmount")
      claimEitherTestNegative shouldBe Left("submitCorrectAmount.invalidAmount")
      claimEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidAmount")
    }

    "submit invalid claim amount for selected tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))

      val claimEitherTestZero     =
        declaration.flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("0")))
      val claimEitherTestNegative =
        declaration.flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("-0.01")))
      val claimEitherTestGreater  =
        declaration.flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("10.01")))

      claimEitherTestZero     shouldBe Left("submitCorrectAmount.invalidAmount")
      claimEitherTestNegative shouldBe Left("submitCorrectAmount.invalidAmount")
      claimEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidAmount")
    }

    "submit invalid correct amount for wrong tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A80, BigDecimal("10.0")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "submit invalid claim amount for wrong tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(TaxCode.A80, BigDecimal("0.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "change to valid amount for selected tax code" in {
      forAll(completeClaimGen) { claim =>
        val totalAmount: BigDecimal              = claim.getTotalReimbursementAmount
        val taxCodes: Seq[(TaxCode, BigDecimal)] = claim.getSelectedTaxCodesWithCorrectAmount
        for (taxCode, correctAmount) <- taxCodes do {
          val newCorrectAmount = correctAmount / 2
          val claimEither      = claim.submitCorrectAmount(taxCode, newCorrectAmount)
          claimEither.isRight shouldBe true
          val modifiedClaim = claimEither.getOrFail
          modifiedClaim.getTotalReimbursementAmount shouldBe (totalAmount + newCorrectAmount)
        }
      }
    }

    "change to invalid amount for selected tax code" in {
      forAll(completeClaimGen) { claim =>
        val taxCodes: Seq[TaxCode] = claim.getSelectedDuties.get

        for taxCode <- taxCodes do {
          val ndrcDetails = claim.getNdrcDetailsFor(taxCode).get
          val newAmount   = BigDecimal(ndrcDetails.amount)
          val claimEither = claim.submitCorrectAmount(taxCode, newAmount)

          claimEither shouldBe Left("submitCorrectAmount.invalidAmount")
        }
      }
    }

    "change to valid amount for the tax code not in ACC14" in {
      forAll(completeClaimGen) { claim =>
        val taxCodeSet   = claim.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val wrongTaxCode = TaxCodes.all.find(taxCode => !taxCodeSet.contains(taxCode)).getOrElse(TaxCode.NI633)
        val claimEither  =
          claim.submitCorrectAmount(wrongTaxCode, BigDecimal("10.00"))
        claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
      }
    }

    "submit CurrentMonthAdjustment as reimbursement method when all duties are CMA eligible" in {
      val importDeclarationAllCMAEligible =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val claimEither                     =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.99")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment))

      claimEither.isRight shouldBe true
    }

    "fail submitting CurrentMonthAdjustment as reimbursement method when NOT all duties are CMA eligible" in {
      val importDeclarationNotCMAEligible =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), false)))
      val claimEither                     =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclarationNotCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment))

      claimEither shouldBe Left("submitReimbursementMethod.notCMAEligible")
    }

    "submit BankAccountTransfer as reimbursement method when all duties are CMA eligible" in {
      val importDeclarationAllCMAEligible =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val claimEither                     =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.01")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))

      claimEither.isRight shouldBe true
    }

    "submit BankAccountTransfer as reimbursement method when NOT all duties are CMA eligible" in {
      val importDeclarationNotCMAEligible =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), false)))
      val claimEither                     =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclarationNotCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))

      claimEither.isRight shouldBe true
    }

    "submit currency type" in {
      val declaration =
        buildImportDeclaration(dutyDetails =
          Seq((TaxCode.A00, BigDecimal("1.00"), false), (TaxCode.A90, BigDecimal("2.00"), true))
        )

      val claim =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00, TaxCode.A90)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
          .flatMap(_.submitCorrectAmount(TaxCode.A90, BigDecimal("1.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))
          .flatMap(_.submitPayeeType(PayeeType.Consignee))
          .flatMap(_.submitCurrencyType(CurrencyType.EUR))
          .getOrFail

      claim.answers.currencyType shouldBe Some(CurrencyType.EUR)
    }

    "submit currency type and get error if not needed" in {
      val declaration =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), false)))

      val claimEither =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))
          .flatMap(_.submitPayeeType(PayeeType.Consignee))
          .flatMap(_.submitCurrencyType(CurrencyType.EUR))

      claimEither shouldBe Left("submitCurrencyType.doesNotNeedCurrencyTypeSelection")
    }

    "computeBankDetails" should {
      "return consigneeBankDetails when payeeType is consignee" in {
        val declaration = buildImportDeclaration(
          dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)),
          consigneeBankDetails = Some(exampleConsigneeBankAccountDetails),
          declarantBankDetails = Some(exampleDeclarantBankAccountDetails)
        )

        val claimEither =
          OverpaymentsSingleClaim
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
            .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
            .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
            .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))
            .flatMap(_.submitPayeeType(PayeeType.Consignee))

        claimEither.isRight                                shouldBe true
        claimEither.toOption.get.computeBankAccountDetails shouldBe Some(exampleConsigneeBankAccountDetails)
      }

      "return declarantBankDetails when payeeType is declarant" in {
        val declaration = buildImportDeclaration(
          dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)),
          consigneeBankDetails = Some(exampleConsigneeBankAccountDetails),
          declarantBankDetails = Some(exampleDeclarantBankAccountDetails)
        )

        val claimEither =
          OverpaymentsSingleClaim
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
            .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
            .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
            .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))
            .flatMap(_.submitPayeeType(PayeeType.Declarant))

        claimEither.isRight                                                shouldBe true
        claimEither.toOption.get.computeBankAccountDetails                 shouldBe Some(exampleDeclarantBankAccountDetails)
        claimEither.getOrFail.submitPayeeType(PayeeType.Declarant).isRight shouldBe true
      }
    }

    "submit bankAccountDetails and bankAccountType if reimbursement method is BankAccountTransfer" in {
      val importDeclarationAllCMAEligible =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val claimEither                     =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.Business))

      claimEither.isRight shouldBe true
    }

    "change reimbursementMethod to CMA in a complete claim with all duties CMA eligible" in {
      forAll(completeClaimCMAEligibleGen) { claim =>
        whenever(claim.needsBanksAccountDetailsSubmission) {
          val modifiedClaim =
            claim
              .submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment)
              .getOrFail

          modifiedClaim.hasCompleteAnswers shouldBe true
        }
      }
    }

    "change bankAccountDetails in a complete claim with all duties CMA eligible" in {
      forAll(completeClaimCMAEligibleGen) { claim =>
        val claimEither =
          claim.submitBankAccountDetails(exampleBankAccountDetails)

        claimEither.isRight shouldBe claim.needsBanksAccountDetailsSubmission
      }
    }

    "change bankAccountDetails in a complete claim not eligible for CMA" in {
      forAll(completeClaimNotCMAEligibleGen) { claim =>
        val claimEither =
          claim.submitBankAccountDetails(exampleBankAccountDetails)

        claimEither.isRight shouldBe true
      }
    }

    "hasCompleteReimbursementClaims" when {
      "return true if all claim amounts are present" in {
        forAll(completeClaimGen) { claim =>
          claim.hasCompleteReimbursementClaims shouldBe true
        }
      }

      "return false if at least one of the claimed tax code do not have a value specified" in {
        forAll(importDeclarationGen, Acc14Gen.genListNdrcDetails()) {
          (importDeclaration: ImportDeclaration, ndrcDetails: List[NdrcDetails]) =>
            whenever(
              ndrcDetails.size > 1 && ndrcDetails.forall(details => BigDecimal(details.amount) > 2) && ndrcDetails
                .map(_.taxType)
                .toSet
                .size == ndrcDetails.size
            ) {
              val taxCodes     = ndrcDetails.map(details => TaxCode(details.taxType))
              val drd          = importDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
              val updatedDd    = importDeclaration.copy(displayResponseDetail = drd)
              val initialClaim = OverpaymentsSingleClaim
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(exampleMrn, updatedDd)
                .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(taxCodes))
                .getOrFail
              val claimToTest  = ndrcDetails.dropRight(1).foldLeft(initialClaim) { case (claim, ndrcDetails) =>
                claim.submitCorrectAmount(TaxCode(ndrcDetails.taxType), 1).getOrFail
              }
              claimToTest.hasCompleteReimbursementClaims shouldBe false
            }
        }
      }

      "return false if no tax codes have been claimed yet" in {
        forAll(importDeclarationGen, Acc14Gen.genListNdrcDetails()) {
          (importDeclaration: ImportDeclaration, ndrcDetails: List[NdrcDetails]) =>
            whenever(
              ndrcDetails.size > 1 && ndrcDetails.forall(details => BigDecimal(details.amount) > 2) && ndrcDetails
                .map(_.taxType)
                .toSet
                .size == ndrcDetails.size
            ) {
              val drd       = importDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
              val updatedDd = importDeclaration.copy(displayResponseDetail = drd)
              val claim     = OverpaymentsSingleClaim
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(exampleMrn, updatedDd)
                .getOrFail

              claim.hasCompleteReimbursementClaims shouldBe false
            }
        }
      }
    }

    "validate if any subsidy payment method is in the declaration" in {

      import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport.withSomeSubsidiesPaymentMethod

      val declaration =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
          .withSomeSubsidiesPaymentMethod()

      val claim = OverpaymentsSingleClaim
        .empty(
          exampleEori,
          features = Some(
            OverpaymentsSingleClaim
              .Features(
                shouldAllowOtherBasisOfClaim = true
              )
          )
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
        .getOrFail

      claim.features shouldBe Some(
        OverpaymentsSingleClaim.Features(
          shouldAllowOtherBasisOfClaim = true
        )
      )

      OverpaymentsSingleClaim.Checks.declarationsHasNoSubsidyPayments.apply(
        claim
      ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)

    }

    "complete claim has new eori and dan when basis of claim is IncorrectEoriAndDan" in {
      forAll(completeClaimCMAEligibleGen) { claim =>
        val modifiedClaim = claim
          .submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectEoriAndDan)
          .submitNewEori(exampleEori)
          .submitNewDan(exampleDan)

        modifiedClaim.hasCompleteAnswers shouldBe true
      }
    }

    "return MISSING_NEW_EORI when complete claim basis of claim is IncorrectEoriAndDan and new eori is not set" in {
      forAll(
        completeClaimGen.suchThat(j => !j.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.IncorrectEoriAndDan))
      ) { claim =>
        val modifiedClaim = claim
          .submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectEoriAndDan)
          .submitNewDan(exampleDan)

        OverpaymentsSingleClaim.Checks.newEoriAndDanProvidedIfNeeded.apply(
          modifiedClaim
        ) shouldBe Validator.Invalid(MISSING_NEW_EORI)
      }
    }

    "return MISSING_NEW_DAN when complete claim basis of claim is IncorrectEoriAndDan and new dan is not set" in {
      forAll(
        completeClaimGen.suchThat(j => !j.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.IncorrectEoriAndDan))
      ) { claim =>
        val modifiedClaim = claim
          .submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectEoriAndDan)
          .submitNewEori(exampleEori)

        OverpaymentsSingleClaim.Checks.newEoriAndDanProvidedIfNeeded.apply(
          modifiedClaim
        ) shouldBe Validator.Invalid(MISSING_NEW_DAN)
      }
    }

    "submit document type selection" in {
      val documentType = UploadDocumentType.overpaymentsSingleDocumentTypes(0)
      val claim        =
        OverpaymentsSingleClaim
          .empty(exampleEori)
          .submitDocumentTypeSelection(documentType)

      claim.answers.selectedDocumentType shouldBe Some(documentType)
    }
  }

  "change document type selection" in {
    val newDocumentType = UploadDocumentType.overpaymentsSingleDocumentTypes(1)
    val claim           =
      OverpaymentsSingleClaim
        .empty(exampleEori)
        .submitDocumentTypeSelection(UploadDocumentType.overpaymentsSingleDocumentTypes.head)

    val modifiedClaim = claim.submitDocumentTypeSelection(newDocumentType)

    modifiedClaim.answers.selectedDocumentType shouldBe Some(newDocumentType)
  }

  "reset reimbursement method" in {
    val claimEither = OverpaymentsSingleClaim
      .empty(exampleEori)
      .submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer)
      .flatMap(_.submitBankAccountType(BankAccountType.Business))
      .flatMap(_.submitBankAccountDetails(BankAccountGen.genBankAccountDetails.sample.get))
      .getOrFail
      .resetReimbursementMethod()

    claimEither.answers.reimbursementMethod shouldBe None
    claimEither.answers.bankAccountType     shouldBe None
    claimEither.answers.bankAccountDetails  shouldBe None
  }

  "remove bank account details" in {
    val claimEither = OverpaymentsSingleClaim
      .empty(exampleEori)
      .submitBankAccountDetails(BankAccountGen.genBankAccountDetails.sample.get)
      .getOrFail
      .removeBankAccountDetails()

    claimEither.answers.bankAccountDetails shouldBe None
  }

  "remove unsupported tax codes" in {
    val claim = OverpaymentsSingleClaim
      .empty(exampleEori)
      .submitMovementReferenceNumberAndDeclaration(
        exampleMrn,
        exampleImportDeclarationWithSomeUnsupportedCode
      )
      .getOrFail
      .removeUnsupportedTaxCodes()

    claim.containsUnsupportedTaxCode shouldBe false
  }

  "return left checking consignee eori with duplicate declaration when consignee eori doesn't match declaration eori" in {
    val importDeclaration          = buildImportDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    )
    val duplicateImportDeclaration = buildImportDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    ).optionallyWithMRN(Some(anotherExampleMrn))
      .withConsigneeEori(anotherExampleEori)
      .withDeclarantEori(yetAnotherExampleEori)
    val claimEither                = OverpaymentsSingleClaim
      .empty(exampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
      .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
      .flatMap(_.submitDuplicateMovementReferenceNumberAndDeclaration(anotherExampleMrn, duplicateImportDeclaration))

    claimEither.getOrFail.checkConsigneeEoriNumberWithDuplicateDeclaration(exampleEori) shouldBe Left(
      "checkConsigneeEoriNumberWithDuplicateDeclaration.shouldMatchConsigneeEoriFromACC14"
    )
  }

  "return left checking declarant eori with duplicate declaration when consignee eori doesn't match declaration eori" in {
    val importDeclaration          = buildImportDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    )
    val duplicateImportDeclaration = buildImportDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    ).optionallyWithMRN(Some(anotherExampleMrn))
      .withConsigneeEori(anotherExampleEori)
      .withDeclarantEori(yetAnotherExampleEori)
    val claimEither                = OverpaymentsSingleClaim
      .empty(exampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
      .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
      .flatMap(_.submitDuplicateMovementReferenceNumberAndDeclaration(anotherExampleMrn, duplicateImportDeclaration))

    claimEither.getOrFail.checkDeclarantEoriNumberWithDuplicateDeclaration(exampleEori) shouldBe Left(
      ClaimValidationErrors.SHOULD_MATCH_ACC14_DUPLICATE_DECLARANT_EORI
    )
  }

  "successfully accept declarant eori with duplicate declaration when no previous consignee check" in {
    val importDeclaration          = buildImportDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    )
    val duplicateImportDeclaration = buildImportDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    ).optionallyWithMRN(Some(anotherExampleMrn))
      .withConsigneeEori(anotherExampleEori)
      .withDeclarantEori(yetAnotherExampleEori)
    val claimEither                = OverpaymentsSingleClaim
      .empty(exampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
      .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
      .flatMap(_.submitDuplicateMovementReferenceNumberAndDeclaration(anotherExampleMrn, duplicateImportDeclaration))

    claimEither.getOrFail
      .checkDeclarantEoriNumberWithDuplicateDeclaration(yetAnotherExampleEori)
      .isRight shouldBe true
  }

  "receive uploaded files" in {
    val claim        = completeClaimGen.sample.get
    val uploadedFile = exampleUploadedFile

    val modifiedClaim = claim
      .receiveUploadedFiles(None, claim.answers.nonce, Seq(uploadedFile))

    modifiedClaim.isRight                             shouldBe true
    modifiedClaim.getOrFail.answers.supportingEvidences should contain theSameElementsAs Seq(uploadedFile)
  }

  "receive uploaded files and set document type if not set" in {
    val claim        = completeClaimGen.sample.get
    val uploadedFile = exampleUploadedFile.copy(cargo = None)
    val newFileType  = UploadDocumentType.overpaymentsSingleDocumentTypes.head

    val modifiedClaim = claim
      .receiveUploadedFiles(Some(newFileType), claim.answers.nonce, Seq(uploadedFile))

    modifiedClaim.getOrFail.answers.supportingEvidences.head.cargo shouldBe Some(newFileType)
  }

  "return left when receiving uploaded files with invalid nonce" in {
    val claim        = completeClaimGen.sample.get
    val uploadedFile = exampleUploadedFile

    claim.receiveUploadedFiles(None, Nonce.random, Seq(uploadedFile)) shouldBe Left(
      "receiveUploadedFiles.invalidNonce"
    )
  }
}
