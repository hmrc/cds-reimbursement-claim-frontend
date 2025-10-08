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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*

class RejectedGoodsSingleClaimSpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "RejectedGoodsSingleClaim" should {
    "have an empty instance" in {
      emptyClaim.answers.userEoriNumber                                         shouldBe exampleEori
      emptyClaim.answers.bankAccountDetails                                     shouldBe None
      emptyClaim.answers.bankAccountType                                        shouldBe None
      emptyClaim.answers.basisOfClaim                                           shouldBe None
      emptyClaim.answers.basisOfClaimSpecialCircumstances                       shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.contactDetails                                         shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber) shouldBe None
      emptyClaim.answers.detailsOfRejectedGoods                                 shouldBe None
      emptyClaim.answers.displayDeclaration                                     shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyClaim.answers.inspectionAddress                                      shouldBe None
      emptyClaim.answers.inspectionDate                                         shouldBe None
      emptyClaim.answers.methodOfDisposal                                       shouldBe None
      emptyClaim.answers.correctedAmounts                                       shouldBe None
      emptyClaim.answers.reimbursementMethod                                    shouldBe None
      emptyClaim.answers.selectedDocumentType                                   shouldBe None
      emptyClaim.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyClaim.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyClaim.getNdrcDetails                                                 shouldBe None
      emptyClaim.getSelectedDuties                                              shouldBe None
      emptyClaim.isAllSelectedDutiesAreCMAEligible                              shouldBe false
      emptyClaim.hasCompleteReimbursementClaims                                 shouldBe false
      emptyClaim.hasCompleteSupportingEvidences                                 shouldBe false
      emptyClaim.hasCompleteAnswers                                             shouldBe false
      emptyClaim.toOutput.isLeft                                                shouldBe true
      emptyClaim.isFinalized                                                    shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeClaimGen) { claim =>
        RejectedGoodsSingleClaim.validator.apply(claim) shouldBe Right(())
        claim.answers.checkYourAnswersChangeMode        shouldBe true
        claim.hasCompleteReimbursementClaims            shouldBe true
        claim.hasCompleteSupportingEvidences            shouldBe true
        claim.hasCompleteAnswers                        shouldBe true
        claim.isFinalized                               shouldBe false

        val output = claim.toOutput.getOrElse(fail("Claim output not defined."))

        output.movementReferenceNumber  shouldBe claim.answers.movementReferenceNumber.get
        output.claimantType             shouldBe claim.getClaimantType
        output.basisOfClaim             shouldBe claim.answers.basisOfClaim.get
        output.methodOfDisposal         shouldBe claim.answers.methodOfDisposal.get
        output.detailsOfRejectedGoods   shouldBe claim.answers.detailsOfRejectedGoods.get
        output.inspectionDate           shouldBe claim.answers.inspectionDate.get
        output.inspectionAddress        shouldBe claim.answers.inspectionAddress.get
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
        RejectedGoodsSingleClaim
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
        RejectedGoodsSingleClaim
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
        RejectedGoodsSingleClaim
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
        RejectedGoodsSingleClaim
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
        RejectedGoodsSingleClaim
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
        RejectedGoodsSingleClaim
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
        RejectedGoodsSingleClaim
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
        RejectedGoodsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val claimEither        =
        RejectedGoodsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "fail if submitted declarant EORI is not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val claimEither        =
        RejectedGoodsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val claimEither        =
        RejectedGoodsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val claim =
          RejectedGoodsSingleClaim
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
        val claim = RejectedGoodsSingleClaim.empty(exampleEori).submitContactAddress(contactAddress)

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
        val claim = RejectedGoodsSingleClaim.empty(exampleEori).submitBasisOfClaim(basisOfClaim)
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
      val claimEither = RejectedGoodsSingleClaim
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
        val claim = RejectedGoodsSingleClaim.empty(exampleEori).submitMethodOfDisposal(methodOfDisposal)
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
      val claim = RejectedGoodsSingleClaim
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

    "select valid tax codes for reimbursement when none yet selected" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00, TaxCode.A90)))

      claimEither.isRight shouldBe true
    }

    "return left when selecting valid tax codes for reimbursement without a display declaration" in {
      val claimEither = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00, TaxCode.A90))

      claimEither shouldBe Left("selectTaxCodeSetForReimbursement.missingDisplayDeclaration")
    }

    "return left when selecting empty list of tax codes for reimbursement" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq.empty))

      claimEither shouldBe Left("selectTaxCodeSetForReimbursement.emptySelection")
    }

    "replace valid tax codes for reimbursement" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))

      claimEither.getOrFail.getSelectedDuties           shouldBe Some(Seq(TaxCode.A00))
      claimEither.getOrFail.answers.reimbursementMethod shouldBe None

      val claimEither2 =
        claimEither.flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A90, TaxCode.A20)))

      claimEither2.getOrFail.getSelectedDuties           shouldBe Some(
        Seq(TaxCode.A90, TaxCode.A20)
      )
      claimEither2.getOrFail.answers.reimbursementMethod shouldBe None
    }

    "replace valid tax codes for reimbursement with non-CMA duty addition when CurrentMonthAdjustment" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
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
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
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
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("1.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
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
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("5.00")))

      claimEither.isRight shouldBe true
    }

    "submit valid claim amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("6.66")))

      claimEither.isRight                               shouldBe true
      claimEither.getOrFail.getTotalReimbursementAmount shouldBe BigDecimal("6.66")
    }

    "submit valid correct amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A80, BigDecimal("5.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "submit valid claim amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(TaxCode.A80, BigDecimal("6.66")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "submit invalid correct amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
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
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))

      val claimEitherTestZero     =
        declaration.flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("0.00")))
      val claimEitherTestNegative =
        declaration.flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("-0.01")))
      val claimEitherTestGreater  =
        declaration.flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("10.01")))

      claimEitherTestZero     shouldBe Left("submitCorrectAmount.invalidAmount")
      claimEitherTestNegative shouldBe Left("submitCorrectAmount.invalidAmount")
      claimEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidAmount")
    }

    "submit invalid correct amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A80, BigDecimal("10.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "submit invalid claim amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(TaxCode.A80, BigDecimal("0.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "return left when submitting valid correct amount with missing display declaration" in {
      val claimEither = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitCorrectAmount(TaxCode.A80, BigDecimal("5.00"))

      claimEither shouldBe Left("submitCorrectAmount.missingDisplayDeclaration")
    }

    "return left when submitting valid claim amount with missing display declaration" in {
      val claimEither = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitClaimAmount(TaxCode.A80, BigDecimal("6.66"))

      claimEither shouldBe Left("submitCorrectAmount.missingDisplayDeclaration")
    }

    "return left when submitting valid correct amount for with no tax code selected" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("5.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotSelectedYet")
    }

    "return left when submitting valid claim amount for with no tax code selected" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither        = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("6.66")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotSelectedYet")
    }

    "change to valid correct amount for selected tax code" in {
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

    "change to invalid correct amount for selected tax code" in {
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

    "change to valid correct amount for the tax code not in ACC14" in {
      forAll(completeClaimGen) { claim =>
        val taxCodeSet   = claim.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val wrongTaxCode = TaxCodes.all.find(taxCode => !taxCodeSet.contains(taxCode)).getOrElse(TaxCode.NI633)
        val claimEither  =
          claim.submitCorrectAmount(wrongTaxCode, BigDecimal("10.00"))
        claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
      }
    }

    "submit inspection date" in {
      forAll(DateGen.genDate) { inspectionDate =>
        val claim = RejectedGoodsSingleClaim.empty(exampleEori).submitInspectionDate(inspectionDate)

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
        val claim = RejectedGoodsSingleClaim.empty(exampleEori).submitInspectionAddress(inspectionAddress)

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

    "submit CurrentMonthAdjustment as reimbursement method when all duties are CMA eligible" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val claimEither                      =
        RejectedGoodsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.99")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment))

      claimEither.isRight shouldBe true
    }

    "fail submitting CurrentMonthAdjustment as reimbursement method when NOT all duties are CMA eligible" in {
      val displayDeclarationNotCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), false)))
      val claimEither                      =
        RejectedGoodsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationNotCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment))

      claimEither shouldBe Left("submitReimbursementMethod.notCMAEligible")
    }

    "submit BankAccountTransfer as reimbursement method when all duties are CMA eligible" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val claimEither                      =
        RejectedGoodsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.01")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))

      claimEither.isRight shouldBe true
    }

    "submit BankAccountTransfer as reimbursement method when NOT all duties are CMA eligible" in {
      val displayDeclarationNotCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), false)))
      val claimEither                      =
        RejectedGoodsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationNotCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))

      claimEither.isRight shouldBe true
    }

    "submit bankAccountDetails and bankAccountType if reimbursement method is BankAccountTransfer" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val claimEither                      =
        RejectedGoodsSingleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
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
        forAll(displayDeclarationGen, Acc14Gen.genListNdrcDetails()) {
          (displayDeclaration: DisplayDeclaration, ndrcDetails: List[NdrcDetails]) =>
            whenever(
              ndrcDetails.size > 1 && ndrcDetails.forall(details => BigDecimal(details.amount) > 2) && ndrcDetails
                .map(_.taxType)
                .toSet
                .size == ndrcDetails.size
            ) {
              val taxCodes     = ndrcDetails.map(details => TaxCode(details.taxType))
              val drd          = displayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
              val updatedDd    = displayDeclaration.copy(displayResponseDetail = drd)
              val initialClaim = RejectedGoodsSingleClaim
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
        forAll(displayDeclarationGen, Acc14Gen.genListNdrcDetails()) {
          (displayDeclaration: DisplayDeclaration, ndrcDetails: List[NdrcDetails]) =>
            whenever(
              ndrcDetails.size > 1 && ndrcDetails.forall(details => BigDecimal(details.amount) > 2) && ndrcDetails
                .map(_.taxType)
                .toSet
                .size == ndrcDetails.size
            ) {
              val drd       = displayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
              val updatedDd = displayDeclaration.copy(displayResponseDetail = drd)
              val claim     = RejectedGoodsSingleClaim
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
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
          .withSomeSubsidiesPaymentMethod()

      val claim = RejectedGoodsSingleClaim
        .empty(
          exampleEori
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
        .getOrFail

      RejectedGoodsSingleClaim.Checks.declarationsHasNoSubsidyPayments.apply(
        claim
      ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)

    }

    "remove bank account details" in {
      val claimEither = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitBankAccountDetails(BankAccountGen.genBankAccountDetails.sample.get)
        .getOrFail
        .removeBankAccountDetails()

      claimEither.answers.bankAccountDetails shouldBe None
    }

    "remove unsupported tax codes" in {
      val claim = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(
          exampleMrn,
          exampleDisplayDeclarationWithSomeUnsupportedCode
        )
        .getOrFail
        .removeUnsupportedTaxCodes()

      claim.containsUnsupportedTaxCode shouldBe false
    }

    "remove bank account details when submitting a different payeeType" in {
      val claim = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitPayeeType(PayeeType.Consignee)
        .getOrFail
        .submitBankAccountDetails(exampleBankAccountDetails)
        .getOrFail
        .submitPayeeType(PayeeType.Declarant)

      claim.getOrFail.answers.bankAccountDetails shouldBe None
    }

    "keep bank account details when submitting the same payeeType" in {
      val claim = RejectedGoodsSingleClaim
        .empty(exampleEori)
        .submitPayeeType(PayeeType.Consignee)
        .getOrFail
        .submitBankAccountDetails(exampleBankAccountDetails)
        .getOrFail
        .submitPayeeType(PayeeType.Consignee)

      claim.getOrFail.answers.bankAccountDetails shouldBe Some(exampleBankAccountDetails)
    }
  }
}
