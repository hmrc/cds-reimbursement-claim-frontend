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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator

class OverpaymentsMultipleClaimSpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "OverpaymentsMultipleClaim" should {
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
      emptyClaim.answers.importDeclarations                                     shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyClaim.answers.correctedAmounts                                       shouldBe None
      emptyClaim.answers.selectedDocumentType                                   shouldBe None
      emptyClaim.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyClaim.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyClaim.getNdrcDetails                                                 shouldBe None
      emptyClaim.isAllSelectedDutiesAreCMAEligible                              shouldBe false
      emptyClaim.hasCompleteReimbursementClaims                                 shouldBe false
      emptyClaim.hasCompleteSupportingEvidences                                 shouldBe false
      emptyClaim.hasCompleteAnswers                                             shouldBe false
      emptyClaim.toOutput.isLeft                                                shouldBe true
      emptyClaim.isFinalized                                                    shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeClaimGen) { claim =>
        OverpaymentsMultipleClaim.validator.apply(claim) shouldBe Right(())
        claim.answers.checkYourAnswersChangeMode         shouldBe true
        claim.hasCompleteReimbursementClaims             shouldBe true
        claim.hasCompleteSupportingEvidences             shouldBe true
        claim.hasCompleteAnswers                         shouldBe true
        claim.isFinalized                                shouldBe false

        val output = claim.toOutput.getOrElse(fail("Claim output not defined."))

        output.movementReferenceNumbers shouldBe claim.answers.movementReferenceNumbers.get
        output.claimantType             shouldBe claim.getClaimantType
        output.basisOfClaim             shouldBe claim.answers.basisOfClaim.get
        output.additionalDetails        shouldBe claim.answers.additionalDetails.get
        output.reimbursementMethod      shouldBe ReimbursementMethod.BankAccountTransfer
        output.supportingEvidences      shouldBe claim.answers.supportingEvidences.map(EvidenceDocument.from)
        output.bankAccountDetails       shouldBe claim.answers.bankAccountDetails
        output.claimantInformation.eori shouldBe claim.answers.userEoriNumber
        output.reimbursementClaims      shouldBe claim.getReimbursementClaims
        output.reimbursementClaims.size shouldBe claim.countOfMovementReferenceNumbers
      }
    }

    "check incompleteness if less than two MRNs" in {
      forAll(buildCompleteClaimGen(minNumberOfMRNs = 1, maxNumberOfMRNs = 1)) { claim =>
        OverpaymentsMultipleClaim.validator(claim).headErrorOption shouldBe Some(
          MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER
        )
        claim.answers.checkYourAnswersChangeMode                   shouldBe false
        claim.hasCompleteReimbursementClaims                       shouldBe true
        claim.hasCompleteSupportingEvidences                       shouldBe true
        claim.hasCompleteAnswers                                   shouldBe false
        claim.isFinalized                                          shouldBe false
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
        claim.answers.movementReferenceNumbers.flatMap(_.headOption).contains(mrn) shouldBe true
        claim.hasCompleteAnswers                                                   shouldBe false
        claim.hasCompleteReimbursementClaims                                       shouldBe false
        claim.hasCompleteSupportingEvidences                                       shouldBe false
        claim.isFinalized                                                          shouldBe false
      }
    }

    "decline submission of a wrong display declaration mrn" in {
      forAll(mrnWithImportDeclarationGen) { case (mrn, decl) =>
        val claimEither = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(mrn, decl.withDeclarationId("foo"))

        claimEither shouldBe Left("submitMovementReferenceNumber.wrongImportDeclarationMrn")
      }
    }

    "decline submission of a wrong display declaration eori" in {
      val importDeclaration  =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val importDeclaration2 =
        buildImportDeclaration(declarantEORI = exampleEori, consigneeEORI = Some(exampleEori))
      val claimEither        =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, exampleMrn, importDeclaration2)

      claimEither shouldBe Left("submitMovementReferenceNumber.wrongImportDeclarationEori")
    }

    "decline submission of a display declaration with an already existing MRN" in {
      val importDeclaration  =
        buildImportDeclaration(id = exampleMrnAsString)
      val importDeclaration2 =
        buildImportDeclaration(id = exampleMrnAsString)
      val claimEither        =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, exampleMrn, importDeclaration2)

      claimEither shouldBe Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
    }

    "decline submission of declaration at a negative index" in {
      forAll(mrnWithImportDeclarationGen) { case (mrn, decl) =>
        val claim = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(-1, mrn, decl)

        claim shouldBe Left("submitMovementReferenceNumber.negativeIndex")
      }
    }

    "decline submission of declaration at an invalid index" in {
      forAll(mrnWithImportDeclarationGen) { case (mrn, decl) =>
        val claim = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(1, mrn, decl)

        claim shouldBe Left("submitMovementReferenceNumber.invalidIndex")
      }
    }

    "accept change of the MRN" in {
      forAll(completeClaimGen, importDeclarationGen) { (claim, decl) =>
        val decl2         = decl.withDeclarationId(exampleMrnAsString)
        val modifiedClaim = claim
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, decl2)
          .getOrFail
        modifiedClaim.answers.importDeclarations.flatMap(_.headOption) shouldBe Some(decl2)
        modifiedClaim.hasCompleteAnswers                               shouldBe false
        modifiedClaim.hasCompleteReimbursementClaims                   shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences                   shouldBe false
      }
    }

    "decline change of the MRN if it already exists at a different index" in {
      val importDeclaration  =
        buildImportDeclaration(id = exampleMrnAsString)
      val importDeclaration2 =
        buildImportDeclaration(id = anotherExampleMrn.value)
      val claimEither        =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, anotherExampleMrn, importDeclaration2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(0, anotherExampleMrn, importDeclaration2)

      claimEither shouldBe Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
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
        modifiedClaim.getLeadImportDeclaration        shouldBe Some(decl2)
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
            claim.getLeadMovementReferenceNumber.get,
            claim.getLeadImportDeclaration.get
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

        claim.answers.movementReferenceNumbers.toList.flatten.contains(exampleMrn) shouldBe true
        claim.answers.importDeclarations.toList.flatten
          .contains(acc14.withDeclarationId(exampleMrnAsString))                   shouldBe true
        claim.hasCompleteAnswers                                                   shouldBe false
        claim.hasCompleteReimbursementClaims                                       shouldBe false
        claim.hasCompleteSupportingEvidences                                       shouldBe false
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
        modifiedClaim.answers.movementReferenceNumbers shouldBe Some(List(exampleMrn))
        modifiedClaim.answers.importDeclarations       shouldBe Some(List(exampleImportDeclaration))
        modifiedClaim.answers.correctedAmounts         shouldBe None
        modifiedClaim.hasCompleteAnswers               shouldBe false
        modifiedClaim.hasCompleteReimbursementClaims   shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences   shouldBe false
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val claim             =
        OverpaymentsMultipleClaim
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
        OverpaymentsMultipleClaim
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
        OverpaymentsMultipleClaim
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
        OverpaymentsMultipleClaim
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
        OverpaymentsMultipleClaim
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
        OverpaymentsMultipleClaim
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
        OverpaymentsMultipleClaim
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
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori)
      val claimEither       =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "fail if submitted declarant EORI is not needed" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleEori)
      val claimEither       =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori)
      val claimEither       =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val claim =
          OverpaymentsMultipleClaim
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
        val claim = OverpaymentsMultipleClaim.empty(exampleEori).submitContactAddress(contactAddress)

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
        val claim = OverpaymentsMultipleClaim.empty(exampleEori).submitBasisOfClaim(basisOfClaim)
        claim.answers.basisOfClaim shouldBe Some(basisOfClaim)
      }
    }

    "submit additional details" in {
      val claim = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitAdditionalDetails("foo bar")

      claim.answers.additionalDetails shouldBe Some("foo bar")
    }

    "change details of rejected goods" in {
      forAll(completeClaimGen, Gen.asciiPrintableStr) { (claim, additionalDetails) =>
        val modifiedClaim = claim.submitAdditionalDetails(additionalDetails)

        modifiedClaim.hasCompleteAnswers shouldBe true

        claim.answers.payeeType                         shouldBe Some(PayeeType.Declarant)
        modifiedClaim.answers.payeeType                 shouldBe Some(PayeeType.Declarant)
        modifiedClaim.toOutput.map(_.additionalDetails) shouldBe Right(additionalDetails)
      }
    }

    "select valid tax codes for reimbursement when none yet selected" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00, TaxCode.A90)))

      claimEither.isRight                                                       shouldBe true
      claimEither.getOrFail.getAmountPaidForIfSelected(exampleMrn, TaxCode.A00) shouldBe Some(BigDecimal("10.00"))
      claimEither.getOrFail.getAmountPaidForIfSelected(exampleMrn, TaxCode.A20) shouldBe None
    }

    "return left when selecting valid tax codes for reimbursement without a display declaration" in {
      val claimEither = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00, TaxCode.A90))

      claimEither shouldBe Left("selectTaxCodeSetForReimbursement.missingImportDeclaration")
    }

    "return left when selecting empty list of tax codes for reimbursement" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq.empty))

      claimEither shouldBe Left("selectTaxCodeSetForReimbursement.emptySelection")
    }

    "replace valid tax codes for reimbursement" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))

      claimEither.getOrFail.getSelectedDuties(exampleMrn) shouldBe Some(Seq(TaxCode.A00))

      val claimEither2 =
        claimEither.flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A90, TaxCode.A20)))

      claimEither2.getOrFail.getSelectedDuties(exampleMrn) shouldBe Some(
        Seq(TaxCode.A90, TaxCode.A20)
      )

    }

    "select invalid tax codes for reimbursement" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("1.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A80)))

      claimEither.isRight shouldBe false
    }

    "change tax code for reimbursement with the same set" in {
      forAll(completeClaimGen) { claim =>
        val mrn = claim.answers.movementReferenceNumbers.get.head

        val modifiedClaimEither = claim.selectAndReplaceTaxCodeSetForReimbursement(
          mrn,
          claim.getSelectedDuties(mrn).get
        )

        val result = modifiedClaimEither.getOrFail
        result.hasCompleteAnswers shouldBe true
      }
    }

    "change tax code for reimbursement with a new valid set" in {
      forAll(completeClaimGen) { claim =>
        val taxCodeSet                  = claim.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val newTaxCodeSet: Seq[TaxCode] = taxCodeSet.take(2).toSeq
        val mrn                         = claim.answers.movementReferenceNumbers.get.head

        val modifiedClaimEither = claim.selectAndReplaceTaxCodeSetForReimbursement(mrn, newTaxCodeSet)

        val result = modifiedClaimEither.getOrFail
        result.getSelectedDuties(mrn).get shouldBe newTaxCodeSet
      }
    }

    "change tax code for reimbursement with a new invalid set" in {
      forAll(completeClaimGen) { claim =>
        val invalidTaxCodeSet   = TaxCodes.all.take(6)
        val mrn                 = claim.answers.movementReferenceNumbers.get.head
        val modifiedClaimEither = claim.selectAndReplaceTaxCodeSetForReimbursement(mrn, invalidTaxCodeSet)
        modifiedClaimEither shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
      }
    }

    "submit valid correct amount for selected tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("5.00")))

      claimEither.isRight shouldBe true
    }

    "submit valid correct amount for wrong tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A80, BigDecimal("5.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "return left when submitting valid correct amount with missing display declaration" in {
      val claimEither = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitCorrectAmount(exampleMrn, TaxCode.A80, BigDecimal("5.00"))

      claimEither shouldBe Left("submitCorrectAmount.missingImportDeclaration")
    }

    "return left when submitting valid correct amount for with no tax code selected" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("5.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotSelectedYet")
    }

    "submit invalid correct amount for selected tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))

      val claimEitherTestZero     =
        declaration.flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("10.00")))
      val claimEitherTestNegative =
        declaration.flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("-10.00")))
      val claimEitherTestGreater  =
        declaration.flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("20.00")))

      claimEitherTestZero     shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
      claimEitherTestNegative shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
      claimEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
    }

    "submit invalid correct amount for wrong tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A80, BigDecimal("0.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "change to valid correct amount for selected tax code" in {
      forAll(completeClaimGen) { claim =>
        for mrn <- claim.getMovementReferenceNumbers.get do {
          val totalAmount: BigDecimal              = claim.getReimbursementAmountForDeclaration(mrn)
          val taxCodes: Seq[(TaxCode, BigDecimal)] = claim.getReimbursementClaimsFor(mrn).toSeq
          for (taxCode, reimbursementAmount) <- taxCodes do {
            val paidAmount         = claim.getAmountPaidFor(mrn, taxCode).get
            val newCorrectedAmount = (paidAmount - reimbursementAmount) / 2
            val claimEither        = claim.submitCorrectAmount(mrn, taxCode, newCorrectedAmount)
            val modifiedClaim      = claimEither.getOrFail
            modifiedClaim.getReimbursementAmountForDeclaration(mrn) shouldBe (totalAmount + newCorrectedAmount)
          }
        }
      }
    }

    "change to invalid correct amount for selected tax code" in {
      forAll(completeClaimGen) { claim =>
        val mrn                                  = claim.answers.movementReferenceNumbers.get.head
        val taxCodes: Seq[(TaxCode, BigDecimal)] = claim.getReimbursementClaimsFor(mrn).toSeq
        for (taxCode, amount) <- taxCodes do {
          val paidAmount   = claim.getAmountPaidFor(mrn, taxCode).get
          val claimEither1 = claim.submitCorrectAmount(mrn, taxCode, paidAmount)
          val claimEither2 = claim.submitCorrectAmount(mrn, taxCode, paidAmount + BigDecimal("0.01"))
          val claimEither3 = claim.submitCorrectAmount(mrn, taxCode, BigDecimal("-0.01"))
          claimEither1 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          claimEither2 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          claimEither3 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
        }
      }
    }

    "change to valid correct amount for the tax code not in ACC14" in {
      forAll(completeClaimGen) { claim =>
        val mrn          = claim.answers.movementReferenceNumbers.get.head
        val taxCodeSet   = claim.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val wrongTaxCode = TaxCodes.all.find(taxCode => !taxCodeSet.contains(taxCode)).getOrElse(TaxCode.NI633)
        val claimEither  = claim.submitCorrectAmount(mrn, wrongTaxCode, BigDecimal("10.00"))
        claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
      }
    }

    "submit valid claim amount for selected tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A00, BigDecimal("6.66")))

      claimEither.isRight                               shouldBe true
      claimEither.getOrFail.getTotalReimbursementAmount shouldBe BigDecimal("6.66")
    }

    "submit valid claim amount for wrong tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A80, BigDecimal("6.66")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "return left when submitting valid claim amount with missing display declaration" in {
      val claimEither = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitClaimAmount(exampleMrn, TaxCode.A80, BigDecimal("5.00"))

      claimEither shouldBe Left("submitCorrectAmount.missingImportDeclaration")
    }

    "return left when submitting valid claim amount for with no tax code selected" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A00, BigDecimal("5.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotSelectedYet")
    }

    "submit invalid claim amount for selected tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))

      val claimEitherTestZero     =
        declaration.flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A00, BigDecimal("0.00")))
      val claimEitherTestNegative =
        declaration.flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A00, BigDecimal("-0.01")))
      val claimEitherTestGreater  =
        declaration.flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A00, BigDecimal("10.01")))

      claimEitherTestZero     shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
      claimEitherTestNegative shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
      claimEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
    }

    "submit invalid claim amount for wrong tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A80, BigDecimal("0.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "change to valid claim amount for selected tax code" in {
      forAll(completeClaimGen) { claim =>
        for mrn <- claim.getMovementReferenceNumbers.get do {
          val totalAmount: BigDecimal              = claim.getReimbursementAmountForDeclaration(mrn)
          val taxCodes: Seq[(TaxCode, BigDecimal)] = claim.getReimbursementClaimsFor(mrn).toSeq
          for (taxCode, reimbursementAmount) <- taxCodes do {
            val newClaimAmount = reimbursementAmount / 2
            val claimEither    = claim.submitClaimAmount(mrn, taxCode, newClaimAmount)
            val modifiedClaim  = claimEither.getOrFail
            modifiedClaim.getReimbursementAmountForDeclaration(mrn) shouldBe (totalAmount - newClaimAmount)
          }
        }
      }
    }

    "change to invalid claim amount for selected tax code" in {
      forAll(completeClaimGen) { claim =>
        val mrn                                  = claim.answers.movementReferenceNumbers.get.head
        val taxCodes: Seq[(TaxCode, BigDecimal)] = claim.getReimbursementClaimsFor(mrn).toSeq
        for (taxCode, amount) <- taxCodes do {
          val paidAmount   = claim.getAmountPaidFor(mrn, taxCode).get
          val claimEither1 = claim.submitClaimAmount(mrn, taxCode, BigDecimal("0.00"))
          val claimEither2 = claim.submitClaimAmount(mrn, taxCode, paidAmount + BigDecimal("0.01"))
          val claimEither3 = claim.submitClaimAmount(mrn, taxCode, BigDecimal("-0.01"))
          claimEither1 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          claimEither2 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          claimEither3 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
        }
      }
    }

    "change to valid claim amount for the tax code not in ACC14" in {
      forAll(completeClaimGen) { claim =>
        val mrn          = claim.answers.movementReferenceNumbers.get.head
        val taxCodeSet   = claim.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val wrongTaxCode = TaxCodes.all.find(taxCode => !taxCodeSet.contains(taxCode)).getOrElse(TaxCode.NI633)
        val claimEither  = claim.submitCorrectAmount(mrn, wrongTaxCode, BigDecimal("10.00"))
        claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
      }
    }

    "get available document types and claim types" in {
      val importDeclaration      = exampleImportDeclaration
      val availableDocumentTypes = UploadDocumentType.overpaymentsMultipleDocumentTypes

      val availableClaimTypes =
        BasisOfOverpaymentClaim
          .excludeNorthernIrelandClaims(false, Some(importDeclaration), isOtherEnabled = true)

      val claim = OverpaymentsMultipleClaim
        .empty(
          exampleEori,
          features = Some(
            OverpaymentsMultipleClaim
              .Features(
                shouldAllowOtherBasisOfClaim = true
              )
          )
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .getOrFail

      claim.getDocumentTypesIfRequired shouldBe Some(availableDocumentTypes)

      claim.getAvailableClaimTypes shouldBe availableClaimTypes

      for document <- availableDocumentTypes do {
        val result = claim.submitDocumentTypeSelection(document)

        result.answers.selectedDocumentType shouldBe Some(document)
      }
    }

    "get available claim types except for Other when OtherBasisOfClaim feature is disabled" in {
      val importDeclaration = exampleImportDeclaration

      val claim = OverpaymentsMultipleClaim
        .empty(
          exampleEori,
          features = Some(
            OverpaymentsMultipleClaim
              .Features(
                shouldAllowOtherBasisOfClaim = false
              )
          )
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
        .getOrFail

      claim.getAvailableClaimTypes.contains(BasisOfOverpaymentClaim.Miscellaneous) shouldBe false
    }

    "change bankAccountDetails in a complete claim" in {
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
              val initialClaim = RejectedGoodsMultipleClaim
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, updatedDd)
                .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, taxCodes))
                .getOrFail
              val claimToTest  = ndrcDetails.dropRight(1).foldLeft(initialClaim) { case (claim, ndrcDetails) =>
                claim.submitCorrectAmount(exampleMrn, TaxCode(ndrcDetails.taxType), 1).getOrFail
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
              val claim     = RejectedGoodsMultipleClaim
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, updatedDd)
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

      val claim = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
        .getOrFail

      OverpaymentsMultipleClaim.Checks.declarationsHasNoSubsidyPayments.apply(
        claim
      ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)

    }

    "remove MRN and display declaration" in {
      val mrnDisplayDec1 = mrnWithImportDeclarationGen.sample.get
      val mrnDisplayDec2 = mrnWithImportDeclarationGen.sample.get
      val mrnDisplayDec3 = mrnWithImportDeclarationGen.sample.get
      val claim          =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(mrnDisplayDec1._1, mrnDisplayDec1._2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, mrnDisplayDec2._1, mrnDisplayDec2._2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(2, mrnDisplayDec3._1, mrnDisplayDec3._2)
          .getOrFail

      claim.getImportDeclarationFor(mrnDisplayDec3._1) shouldBe Some(mrnDisplayDec3._2)

      val modifiedClaim = claim.removeMovementReferenceNumberAndImportDeclaration(mrnDisplayDec3._1)

      modifiedClaim.getOrFail.getImportDeclarationFor(mrnDisplayDec3._1) shouldBe None
    }

    "return left when attempting to remove first MRN and display declaration" in {
      val mrnDisplayDec1 = mrnWithImportDeclarationGen.sample.get
      val mrnDisplayDec2 = mrnWithImportDeclarationGen.sample.get
      val claim          =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(mrnDisplayDec1._1, mrnDisplayDec1._2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, mrnDisplayDec2._1, mrnDisplayDec2._2)
          .getOrFail

      claim.removeMovementReferenceNumberAndImportDeclaration(mrnDisplayDec1._1) shouldBe Left(
        "removeMovementReferenceNumberAndImportDeclaration.cannotRemoveFirstMRN"
      )
    }

    "return left when attempting to remove second MRN and display declaration" in {
      val mrnDisplayDec1 = mrnWithImportDeclarationGen.sample.get
      val mrnDisplayDec2 = mrnWithImportDeclarationGen.sample.get
      val claim          =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(mrnDisplayDec1._1, mrnDisplayDec1._2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, mrnDisplayDec2._1, mrnDisplayDec2._2)
          .getOrFail

      claim.removeMovementReferenceNumberAndImportDeclaration(mrnDisplayDec2._1) shouldBe Left(
        "removeMovementReferenceNumberAndImportDeclaration.cannotRemoveSecondMRN"
      )
    }

    "return left when attempting to remove MRN and display declaration that doesn't exist" in {
      val mrnDisplayDec1 = mrnWithImportDeclarationGen.sample.get
      val mrnDisplayDec2 = mrnWithImportDeclarationGen.sample.get
      val claim          =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(mrnDisplayDec1._1, mrnDisplayDec1._2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, mrnDisplayDec2._1, mrnDisplayDec2._2)
          .getOrFail

      claim.removeMovementReferenceNumberAndImportDeclaration(exampleMrn) shouldBe Left(
        "removeMovementReferenceNumberAndImportDeclaration.notFound"
      )
    }

    "remove unsupported tax codes" in {
      val claim = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(
          exampleMrn,
          exampleImportDeclarationWithSomeUnsupportedCode
        )
        .getOrFail
        .removeUnsupportedTaxCodes()

      claim.containsUnsupportedTaxCode shouldBe false
    }

    "remove bank account details" in {
      val claimEither = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitBankAccountDetails(BankAccountGen.genBankAccountDetails.sample.get)
        .getOrFail
        .removeBankAccountDetails()

      claimEither.answers.bankAccountDetails shouldBe None
    }

    "remove bank account details when submitting a different payeeType" in {
      val claim = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitPayeeType(PayeeType.Consignee)
        .getOrFail
        .submitBankAccountDetails(exampleBankAccountDetails)
        .getOrFail
        .submitPayeeType(PayeeType.Declarant)

      claim.getOrFail.answers.bankAccountDetails shouldBe None
    }

    "keep bank account details when submitting the same payeeType" in {
      val claim = OverpaymentsMultipleClaim
        .empty(exampleEori)
        .submitPayeeType(PayeeType.Consignee)
        .getOrFail
        .submitBankAccountDetails(exampleBankAccountDetails)
        .getOrFail
        .submitPayeeType(PayeeType.Consignee)

      claim.getOrFail.answers.bankAccountDetails shouldBe Some(exampleBankAccountDetails)
    }

    "get display declaration by index" in {
      val importDeclaration  = buildImportDeclaration(id = exampleMrnAsString)
      val importDeclaration2 = buildImportDeclaration(id = anotherExampleMrn.value)
      val claim              =
        OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, anotherExampleMrn, importDeclaration2)
          .getOrFail

      claim.getNthImportDeclaration(1) shouldBe Some(importDeclaration2)
    }

    "needsDuplicateMrnAndDeclaration" when {
      "basis of claim is not DuplicateEntry return false" in {
        val claim = OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectEoriAndDan)

        claim.needsDuplicateMrnAndDeclaration shouldBe false
      }

      "basis of claim is DuplicateEntry return true" in {
        val claim = OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry)

        claim.needsDuplicateMrnAndDeclaration shouldBe true
      }
    }

    "isAllSelectedDutiesAreCMAEligible" when {
      "all entries are CMA eligible return true" in {
        val displayDec = importDeclarationCMAEligibleGen.sample.get

        val claim = OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDec.getMRN, displayDec)
          .getOrFail

        claim.isAllSelectedDutiesAreCMAEligible                    shouldBe true
        claim.isAllSelectedDutiesAreCMAEligible(displayDec.getMRN) shouldBe true
      }

      "not all entries are CMA eligible return false" in {
        val displayDecCMA    = importDeclarationCMAEligibleGen.sample.get
        val displayDecNotCMA = importDeclarationNotCMAEligibleGen.sample.get

        val claim = OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDecCMA.getMRN, displayDecCMA)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(displayDecNotCMA.getMRN, displayDecNotCMA)
          .getOrFail

        claim.isAllSelectedDutiesAreCMAEligible                          shouldBe false
        claim.isAllSelectedDutiesAreCMAEligible(displayDecNotCMA.getMRN) shouldBe false
      }
    }

    "getAvailableDuties" when {
      "there are available duties return tax code with cmaEligible" in {
        val details         = Seq((TaxCode.A00, BigDecimal(200), true))
        val expectedDetails = Seq((TaxCode.A00, true))
        val displayDec      = buildImportDeclaration(dutyDetails = details)

        val claim = OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDec.getMRN, displayDec)
          .getOrFail

        claim.getAvailableDuties(displayDec.getMRN) shouldBe expectedDetails
      }

      "there are no duties return empty sequence" in {
        val displayDec = buildImportDeclaration(dutyDetails = Seq.empty)

        val claim = OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDec.getMRN, displayDec)
          .getOrFail

        claim.getAvailableDuties(displayDec.getMRN) shouldBe Seq.empty
      }

      "there are no matching duties return empty sequence" in {
        val details    = Seq((TaxCode("foo"), BigDecimal(200), true))
        val displayDec = buildImportDeclaration(dutyDetails = details)

        val claim = OverpaymentsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDec.getMRN, displayDec)
          .getOrFail

        claim.getAvailableDuties(displayDec.getMRN) shouldBe Seq.empty
      }
    }
  }
}
