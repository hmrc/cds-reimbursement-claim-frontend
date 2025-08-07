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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator
import org.scalacheck.Gen
import org.scalacheck.ShrinkLowPriority
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyValidationErrors.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*

class RejectedGoodsScheduledJourneySpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "RejectedGoodsScheduledJourney" should {
    "have an empty instance" in {
      emptyJourney.answers.userEoriNumber                                         shouldBe exampleEori
      emptyJourney.answers.bankAccountDetails                                     shouldBe None
      emptyJourney.answers.bankAccountType                                        shouldBe None
      emptyJourney.answers.basisOfClaim                                           shouldBe None
      emptyJourney.answers.basisOfClaimSpecialCircumstances                       shouldBe None
      emptyJourney.answers.contactAddress                                         shouldBe None
      emptyJourney.answers.contactDetails                                         shouldBe None
      emptyJourney.answers.contactAddress                                         shouldBe None
      emptyJourney.answers.eoriNumbersVerification                                shouldBe None
      emptyJourney.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber) shouldBe None
      emptyJourney.answers.detailsOfRejectedGoods                                 shouldBe None
      emptyJourney.answers.displayDeclaration                                     shouldBe None
      emptyJourney.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyJourney.answers.inspectionAddress                                      shouldBe None
      emptyJourney.answers.inspectionDate                                         shouldBe None
      emptyJourney.answers.methodOfDisposal                                       shouldBe None
      emptyJourney.answers.correctedAmounts                                       shouldBe None
      emptyJourney.answers.selectedDocumentType                                   shouldBe None
      emptyJourney.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyJourney.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyJourney.getSelectedDutyTypes                                           shouldBe None
      emptyJourney.getReimbursementClaims                                         shouldBe Map.empty
      emptyJourney.hasCompleteReimbursementClaims                                 shouldBe false
      emptyJourney.hasCompleteSupportingEvidences                                 shouldBe false
      emptyJourney.hasCompleteAnswers                                             shouldBe false
      emptyJourney.toOutput.isLeft                                                shouldBe true
      emptyJourney.isFinalized                                                    shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeJourneyGen) { journey =>
        RejectedGoodsScheduledJourney.validator.apply(journey) shouldBe Right(())
        journey.answers.checkYourAnswersChangeMode             shouldBe true
        journey.hasCompleteReimbursementClaims                 shouldBe true
        journey.hasCompleteSupportingEvidences                 shouldBe true
        journey.hasCompleteAnswers                             shouldBe true
        journey.isFinalized                                    shouldBe false

        val output = journey.toOutput.getOrElse(fail("Journey output not defined."))

        output.movementReferenceNumber  shouldBe journey.answers.movementReferenceNumber.get
        output.claimantType             shouldBe journey.getClaimantType
        output.basisOfClaim             shouldBe journey.answers.basisOfClaim.get
        output.methodOfDisposal         shouldBe journey.answers.methodOfDisposal.get
        output.detailsOfRejectedGoods   shouldBe journey.answers.detailsOfRejectedGoods.get
        output.inspectionDate           shouldBe journey.answers.inspectionDate.get
        output.inspectionAddress        shouldBe journey.answers.inspectionAddress.get
        output.reimbursementMethod      shouldBe ReimbursementMethod.BankAccountTransfer
        output.reimbursementClaims      shouldBe journey.getReimbursementClaims
        output.scheduledDocument        shouldBe EvidenceDocument.from(journey.answers.scheduledDocument.get)
        output.supportingEvidences.size shouldBe journey.answers.supportingEvidences.size
        output.bankAccountDetails       shouldBe journey.answers.bankAccountDetails
        output.claimantInformation.eori shouldBe journey.answers.userEoriNumber
      }
    }

    "finalize journey with caseNumber" in {
      forAll(completeJourneyGen) { journey =>
        journey.hasCompleteReimbursementClaims shouldBe true
        journey.hasCompleteSupportingEvidences shouldBe true
        journey.hasCompleteAnswers             shouldBe true
        journey.isFinalized                    shouldBe false
        val result          = journey.finalizeJourneyWith("foo-123-abc")
        val modifiedJourney = result.getOrFail
        modifiedJourney.isFinalized                    shouldBe true
        modifiedJourney.hasCompleteReimbursementClaims shouldBe true
        modifiedJourney.hasCompleteSupportingEvidences shouldBe true
        modifiedJourney.hasCompleteAnswers             shouldBe true
        modifiedJourney.finalizeJourneyWith("bar")     shouldBe Left(JOURNEY_ALREADY_FINALIZED)
      }
    }

    "accept submission of a new MRN" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, decl) =>
        val journey = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(mrn, decl)
          .getOrFail
        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteReimbursementClaims                shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe false
        journey.isFinalized                                   shouldBe false
      }
    }

    "decline submission of a wrong display declaration" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, decl) =>
        val journeyEither = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(mrn, decl.withDeclarationId("foo"))

        journeyEither shouldBe Left("submitMovementReferenceNumber.wrongDisplayDeclarationMrn")
      }
    }

    "accept change of the MRN" in {
      forAll(completeJourneyGen, displayDeclarationGen) { (journey, decl) =>
        val decl2           = decl.withDeclarationId(exampleMrnAsString)
        val modifiedJourney = journey
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, decl2)
          .getOrFail
        modifiedJourney.answers.displayDeclaration     shouldBe Some(decl2)
        modifiedJourney.hasCompleteAnswers             shouldBe false
        modifiedJourney.hasCompleteReimbursementClaims shouldBe false
        modifiedJourney.hasCompleteSupportingEvidences shouldBe false
      }
    }

    "accept change of the MRN when user has XI eori" in {
      forAll(
        completeJourneyGen.map(_.submitUserXiEori(UserXiEori(exampleXIEori.value))),
        displayDeclarationGen
      ) { (journey, decl) =>
        val decl2           = decl.withDeclarationId(exampleMrnAsString)
        val modifiedJourney = journey
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, decl2)
          .getOrFail
        modifiedJourney.answers.displayDeclaration      shouldBe Some(decl2)
        modifiedJourney.hasCompleteAnswers              shouldBe false
        modifiedJourney.hasCompleteReimbursementClaims  shouldBe false
        modifiedJourney.hasCompleteSupportingEvidences  shouldBe false
        modifiedJourney.answers.eoriNumbersVerification shouldBe Some(
          EoriNumbersVerification(userXiEori = Some(UserXiEori(exampleXIEori.value)))
        )
      }
    }

    "accept submission of the same MRN" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourney = journey
          .submitMovementReferenceNumberAndDeclaration(
            journey.answers.movementReferenceNumber.get,
            journey.answers.displayDeclaration.get
          )
          .getOrFail
        modifiedJourney                                shouldBe journey
        modifiedJourney.hasCompleteAnswers             shouldBe true
        modifiedJourney.hasCompleteReimbursementClaims shouldBe true
        modifiedJourney.hasCompleteSupportingEvidences shouldBe true
      }
    }

    "accept submission of a new ACC14 data" in {
      forAll(displayDeclarationGen) { acc14 =>
        val journey = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(
            exampleMrn,
            acc14.withDeclarationId(exampleMrnAsString)
          )
          .getOrFail

        journey.answers.movementReferenceNumber.contains(exampleMrn)                             shouldBe true
        journey.answers.displayDeclaration.contains(acc14.withDeclarationId(exampleMrnAsString)) shouldBe true
        journey.hasCompleteAnswers                                                               shouldBe false
        journey.hasCompleteReimbursementClaims                                                   shouldBe false
        journey.hasCompleteSupportingEvidences                                                   shouldBe false
      }
    }

    "accept change of the ACC14 data" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourney =
          journey
            .submitMovementReferenceNumberAndDeclaration(
              exampleMrn,
              exampleDisplayDeclaration
            )
            .getOrFail
        modifiedJourney.answers.movementReferenceNumber shouldBe Some(exampleMrn)
        modifiedJourney.answers.displayDeclaration      shouldBe Some(exampleDisplayDeclaration)
        modifiedJourney.answers.correctedAmounts        shouldBe None
        modifiedJourney.hasCompleteAnswers              shouldBe false
        modifiedJourney.hasCompleteReimbursementClaims  shouldBe false
        modifiedJourney.hasCompleteSupportingEvidences  shouldBe false
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val journey            =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe true
      journey.getClaimantType                          shouldBe ClaimantType.User
      journey.getClaimantEori                          shouldBe exampleEori
    }

    "needs XI eori submission if user's eori not matching those of ACC14 and ACC14 contains XI eori" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val journey            =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail

      exampleXIEori.isXiEori                           shouldBe true
      anotherExampleEori.isXiEori                      shouldBe false
      journey.userHasGBEoriMatchingDeclaration         shouldBe false
      journey.userHasXIEoriMatchingDeclaration         shouldBe false
      journey.needsUserXiEoriSubmission                shouldBe true
      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe true

      val journey2 = journey.submitUserXiEori(UserXiEori(exampleXIEori.value))

      journey2.userHasGBEoriMatchingDeclaration         shouldBe false
      journey2.userHasXIEoriMatchingDeclaration         shouldBe true
      journey2.needsUserXiEoriSubmission                shouldBe false
      journey2.needsDeclarantAndConsigneeEoriSubmission shouldBe false
    }

    "does not need declarant and consignee submission if user's eori is matching that of declarant" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori, consigneeEORI = None)
      val journey            =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Declarant
      journey.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's eori is matching that of consignee" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleEori))
      val journey            =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Consignee
      journey.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is missing" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleXIEori, consigneeEORI = None)
      val journey            =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Declarant
      journey.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is present" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleXIEori, consigneeEORI = Some(anotherExampleEori))
      val journey            =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Declarant
      journey.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of consignee" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val journey            =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Consignee
      journey.getClaimantEori                          shouldBe exampleXIEori
    }

    "fail building journey if user's eori is not matching those of ACC14 and separate EORIs were not provided by the user" in {
      val journeyGen = buildJourneyGen(
        acc14DeclarantMatchesUserEori = false,
        acc14ConsigneeMatchesUserEori = false,
        submitDeclarantDetails = false,
        submitConsigneeDetails = false
      )
      forAll(journeyGen) { result =>
        val journey = result.getOrElse(fail("Journey building has failed."))
        journey.hasCompleteAnswers shouldBe false
      }
    }

    "fail if submitted consignee EORI is not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val journeyEither      =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "fail if submitted declarant EORI is not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val journeyEither      =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val journey =
          RejectedGoodsScheduledJourney
            .empty(exampleEori)
            .submitContactDetails(Some(contactDetails))

        journey.answers.contactDetails shouldBe Some(contactDetails)
      }
    }

    "change contact details" in {
      forAll(completeJourneyGen, ContactDetailsGen.genMrnContactDetails) { (journey, contactDetails) =>
        val modifiedJourney = journey.submitContactDetails(Some(contactDetails))

        modifiedJourney.hasCompleteAnswers     shouldBe true
        modifiedJourney.answers.contactDetails shouldBe Some(
          contactDetails
        )
      }
    }

    "submit contact address" in {
      forAll(ContactAddressGen.genContactAddress) { contactAddress =>
        val journey = RejectedGoodsScheduledJourney.empty(exampleEori).submitContactAddress(contactAddress)

        journey.answers.contactAddress shouldBe Some(contactAddress)
      }
    }

    "change contact address" in {
      forAll(completeJourneyGen, ContactAddressGen.genContactAddress) { (journey, contactAddress) =>
        val modifiedJourney = journey.submitContactAddress(contactAddress)

        modifiedJourney.hasCompleteAnswers     shouldBe true
        modifiedJourney.answers.contactAddress shouldBe Some(
          contactAddress.computeChanges(journey.getInitialAddressDetailsFromDeclaration)
        )
      }
    }

    "submit basis of claim" in {
      forAll(Gen.oneOf(BasisOfRejectedGoodsClaim.values)) { basisOfClaim =>
        val journey = RejectedGoodsScheduledJourney.empty(exampleEori).submitBasisOfClaim(basisOfClaim)
        journey.answers.basisOfClaim shouldBe Some(basisOfClaim)
      }
    }

    "change basis of claim" in {
      forAll(completeJourneyGen, Gen.oneOf(BasisOfRejectedGoodsClaim.allButSpecialCircumstances)) {
        (journey, basisOfClaim) =>
          val modifiedJourney = journey.submitBasisOfClaim(basisOfClaim)

          modifiedJourney.hasCompleteAnswers           shouldBe true
          modifiedJourney.toOutput.map(_.basisOfClaim) shouldBe Right(basisOfClaim)
      }
    }

    "change basis of claim if special circumstances" in {
      forAll(completeJourneyGenWithoutSpecialCircumstances) { journey =>
        val modifiedJourney = journey.submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)

        modifiedJourney.hasCompleteAnswers           shouldBe false
        modifiedJourney.toOutput.map(_.basisOfClaim) shouldBe Left(
          BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_BE_DEFINED :: Nil
        )
      }
    }

    "submit basis of claim special circumstances details" in {
      val journeyEither = RejectedGoodsScheduledJourney
        .empty(exampleEori)
        .submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
        .submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails)

      journeyEither.isRight shouldBe true

    }

    "change basis of claim special circumstances details" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourneyEither =
          journey.submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails)

        modifiedJourneyEither.isRight shouldBe journey.needsSpecialCircumstancesBasisOfClaim
      }
    }

    "submit method of disposal" in {
      forAll(Gen.oneOf(MethodOfDisposal.values)) { methodOfDisposal =>
        val journey = RejectedGoodsScheduledJourney.empty(exampleEori).submitMethodOfDisposal(methodOfDisposal)
        journey.answers.methodOfDisposal shouldBe Some(methodOfDisposal)
      }
    }

    "change method of disposal" in {
      forAll(completeJourneyGen, Gen.oneOf(MethodOfDisposal.values)) { (journey, methodOfDisposal) =>
        val modifiedJourney = journey.submitMethodOfDisposal(methodOfDisposal)

        modifiedJourney.hasCompleteAnswers               shouldBe true
        modifiedJourney.toOutput.map(_.methodOfDisposal) shouldBe Right(methodOfDisposal)
      }
    }

    "submit details of rejected goods" in {
      val journey = RejectedGoodsScheduledJourney
        .empty(exampleEori)
        .submitDetailsOfRejectedGoods(exampleRejectedGoodsDetails)

      journey.answers.detailsOfRejectedGoods shouldBe Some(exampleRejectedGoodsDetails)
    }

    "change details of rejected goods" in {
      forAll(completeJourneyGen, exampleRejectedGoodsDetails) { (journey, rejectedGoodsDetails) =>
        val modifiedJourney = journey.submitDetailsOfRejectedGoods(rejectedGoodsDetails)

        modifiedJourney.hasCompleteAnswers                     shouldBe true
        modifiedJourney.toOutput.map(_.detailsOfRejectedGoods) shouldBe Right(rejectedGoodsDetails)
      }
    }

    "select duty types for reimbursement when none yet selected" in {
      forAll(dutyTypesGen) { (dutyTypes: Seq[DutyType]) =>
        val journey = RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .getOrFail

        journey.getSelectedDutyTypes shouldBe Some(dutyTypes)
      }
    }

    "replace duty types for reimbursement" in {
      forAll(dutyTypesGen, dutyTypesGen) { (dutyTypes1: Seq[DutyType], dutyTypes2: Seq[DutyType]) =>
        val journey = RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes1)
          .flatMap(_.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes2))
          .getOrFail

        journey.getSelectedDutyTypes shouldBe Some(dutyTypes2)
      }
    }

    "select valid tax codes for reimbursement when none yet selected" in {
      forAll(dutyTypesWithTaxCodesGen) { (dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]) =>
        val dutyTypes = dutyTypesWithTaxCodes.map(_._1)
        val journey   = RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            journey => (tc: (DutyType, Seq[TaxCode])) => journey.selectAndReplaceTaxCodeSetForDutyType(tc._1, tc._2)
          )
          .getOrFail

        journey.getSelectedDutyTypes shouldBe Some(dutyTypes)
        dutyTypesWithTaxCodes.foreach { case (dutyType, taxCodes) =>
          journey.getSelectedDutiesFor(dutyType) shouldBe Some(taxCodes)
          taxCodes.foreach(taxCode => journey.isDutySelected(dutyType, taxCode))
          TaxCodes.allExcept(taxCodes.toSet).foreach(taxCode => !journey.isDutySelected(dutyType, taxCode))
        }
      }
    }

    "replace valid tax codes for reimbursement" in {
      DutyTypes.all.foreach { dutyType =>
        forAll(taxCodesGen(dutyType), taxCodesGen(dutyType)) { (taxCodes1, taxCodes2) =>
          val journey = RejectedGoodsScheduledJourney
            .empty(exampleEori)
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .flatMapEach(
              Seq(dutyType -> taxCodes1),
              journey => (tc: (DutyType, Seq[TaxCode])) => journey.selectAndReplaceTaxCodeSetForDutyType(tc._1, tc._2)
            )
            .flatMapEach(
              Seq(dutyType -> taxCodes2),
              journey => (tc: (DutyType, Seq[TaxCode])) => journey.selectAndReplaceTaxCodeSetForDutyType(tc._1, tc._2)
            )
            .getOrFail

          journey.getSelectedDutyTypes.get       shouldBe Seq(dutyType)
          journey.getSelectedDutiesFor(dutyType) shouldBe Some(taxCodes2)
          taxCodes2.foreach(taxCode => journey.isDutySelected(dutyType, taxCode))
          TaxCodes.allExcept(taxCodes2.toSet).foreach(taxCode => !journey.isDutySelected(dutyType, taxCode))
        }
      }
    }

    "select invalid tax codes for reimbursement" in {
      forAll(dutyTypesWithTaxCodesGen) { (dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]) =>
        val dutyTypes = dutyTypesWithTaxCodes.map(_._1)
        val result    = RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            journey =>
              (tc: (DutyType, Seq[TaxCode])) =>
                journey.selectAndReplaceTaxCodeSetForDutyType(tc._1, Seq(TaxCode.A00, TaxCode.A50))
          )

        result shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesDoesNotMatchDutyType")
      }
    }

    "select invalid duty types for reimbursement" in {
      forAll(dutyTypesWithTaxCodesGen) { (dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]) =>
        val dutyTypes = dutyTypesWithTaxCodes.map(_._1)
        val result    = RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(DutyTypes.all.toSet.diff(dutyTypes.toSet).toSeq)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            journey => (tc: (DutyType, Seq[TaxCode])) => journey.selectAndReplaceTaxCodeSetForDutyType(tc._1, tc._2)
          )

        result shouldBe Left("selectTaxCodeSetForReimbursement.dutyTypeNotSelectedBefore")
      }
    }

    "change duty types for reimbursement with the same set" in {
      forAll(completeJourneyGen) { journey =>
        val result = journey
          .selectAndReplaceDutyTypeSetForReimbursement(journey.getSelectedDutyTypes.get)
          .getOrFail
        result.hasCompleteReimbursementClaims shouldBe true
        result.hasCompleteAnswers             shouldBe true
      }
    }

    def isSubset[A](a: Seq[A], b: Seq[A]): Boolean =
      a.toSet.intersect(b.toSet) === a.toSet

    "change duty types for reimbursement with a new valid set" in {
      forAll(completeJourneyGen, dutyTypesGen) { (journey, newDutyTypes) =>
        val result = journey
          .selectAndReplaceDutyTypeSetForReimbursement(newDutyTypes)
          .getOrFail
        result.getSelectedDutyTypes.get       shouldBe newDutyTypes
        result.hasCompleteReimbursementClaims shouldBe isSubset(newDutyTypes, journey.getSelectedDutyTypes.get)
      }
    }

    "change tax codes for reimbursement with the same set" in {
      forAll(completeJourneyGen) { journey =>
        journey.getSelectedDutyTypes.get.foreach { dutyType =>
          val taxCodes = journey.getSelectedDutiesFor(dutyType).get
          val result   = journey
            .selectAndReplaceTaxCodeSetForDutyType(dutyType, taxCodes)
            .getOrFail
          result.hasCompleteReimbursementClaims shouldBe true
          result.hasCompleteAnswers             shouldBe true
        }
      }
    }

    "change tax codes for reimbursement with the same sets" in {
      forAll(completeJourneyGen) { journey =>
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])] =
          journey.getSelectedDutyTypes.get.map(dutyType => dutyType -> journey.getSelectedDutiesFor(dutyType).get)

        val result = journey
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

        val journey = RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            (j: RejectedGoodsScheduledJourney) =>
              { case (dutyType: DutyType, taxCodeSeq: Seq[TaxCode]) =>
                j.selectAndReplaceTaxCodeSetForDutyType(dutyType, taxCodeSeq)
              }: (((DutyType, Seq[TaxCode])) => Either[String, RejectedGoodsScheduledJourney])
          )
          .flatMapEach(
            taxCodesWithAmounts,
            (j: RejectedGoodsScheduledJourney) =>
              { case (dutyType: DutyType, taxCode: TaxCode, paidAmount: BigDecimal, correctAmount: BigDecimal) =>
                j.submitCorrectAmount(dutyType, taxCode, paidAmount, correctAmount)
              }: (((DutyType, TaxCode, BigDecimal, BigDecimal)) => Either[String, RejectedGoodsScheduledJourney])
          )
          .getOrFail

        journey.getSelectedDutyTypes shouldBe Some(dutyTypes)

        dutyTypesWithTaxCodes.foreach { case (dutyType, taxCodes) =>
          journey.getSelectedDutiesFor(dutyType).get shouldBe taxCodes
          taxCodes.foreach(taxCode => journey.isDutySelected(dutyType, taxCode))
          TaxCodes.allExcept(taxCodes.toSet).foreach(taxCode => !journey.isDutySelected(dutyType, taxCode))
        }

        journey.getReimbursementClaims.map(_._2.size).sum shouldBe taxCodesWithAmounts.size
        journey.getTotalReimbursementAmount               shouldBe expectedTotalReimbursementAmount
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

        val journey = RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            (j: RejectedGoodsScheduledJourney) =>
              { case (dutyType: DutyType, taxCodeSeq: Seq[TaxCode]) =>
                j.selectAndReplaceTaxCodeSetForDutyType(dutyType, taxCodeSeq)
              }: (((DutyType, Seq[TaxCode])) => Either[String, RejectedGoodsScheduledJourney])
          )
          .flatMapEach(
            taxCodesWithAmounts,
            (j: RejectedGoodsScheduledJourney) =>
              { case (dutyType: DutyType, taxCode: TaxCode, paidAmount: BigDecimal, correctAmount: BigDecimal) =>
                j.submitClaimAmount(dutyType, taxCode, paidAmount, paidAmount - correctAmount)
              }: (((DutyType, TaxCode, BigDecimal, BigDecimal)) => Either[String, RejectedGoodsScheduledJourney])
          )
          .getOrFail

        journey.getSelectedDutyTypes shouldBe Some(dutyTypes)

        dutyTypesWithTaxCodes.foreach { case (dutyType, taxCodes) =>
          journey.getSelectedDutiesFor(dutyType).get shouldBe taxCodes
          taxCodes.foreach(taxCode => journey.isDutySelected(dutyType, taxCode))
          TaxCodes.allExcept(taxCodes.toSet).foreach(taxCode => !journey.isDutySelected(dutyType, taxCode))
        }

        journey.getReimbursementClaims.map(_._2.size).sum shouldBe taxCodesWithAmounts.size
        journey.getTotalReimbursementAmount               shouldBe expectedTotalReimbursementAmount
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

        val result = RejectedGoodsScheduledJourney
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

        val result = RejectedGoodsScheduledJourney
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

        val result = RejectedGoodsScheduledJourney
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
      forAll(completeJourneyGen) { journey =>
        val totalReimbursementAmount = journey.getTotalReimbursementAmount
        val totalPaidAmount          = journey.getTotalPaidAmount

        journey.getReimbursementClaims.foreachEntry { case (dutyType, tca) =>
          tca.foreachEntry { case (taxCode, AmountPaidWithCorrect(pa, ca)) =>
            val modifiedJourney =
              journey.submitCorrectAmount(dutyType, taxCode, pa * 0.8, ca * 0.8).getOrFail

            modifiedJourney.getTotalReimbursementAmount shouldBe totalReimbursementAmount - ((pa - ca) * 0.2)
            modifiedJourney.getTotalPaidAmount          shouldBe totalPaidAmount - pa * 0.2

          }
        }
      }
    }

    "reject change to valid amount for not selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        def taxCodeNotSelected(dutyType: DutyType): TaxCode =
          journey
            .getSelectedDutiesFor(dutyType)
            .map(tcs => dutyType.taxCodes.filterNot(tcs.contains).iterator.next())
            .getOrElse(fail())

        journey.getReimbursementClaims.foreachEntry { case (dutyType, tca) =>
          tca.foreachEntry { case (_, AmountPaidWithCorrect(pa, ca)) =>
            val result =
              journey.submitCorrectAmount(dutyType, taxCodeNotSelected(dutyType), pa, ca)
            result shouldBe Left("submitAmountForReimbursement.taxCodeNotSelected")
          }
        }
      }
    }

    "reject change to invalid amount for valid selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        journey.getReimbursementClaims.foreachEntry { case (dutyType, tca) =>
          tca.foreachEntry { case (taxCode, AmountPaidWithCorrect(pa, _)) =>
            val result1 =
              journey.submitCorrectAmount(dutyType, taxCode, pa, pa)
            result1 shouldBe Left("submitAmountForReimbursement.invalidReimbursementAmount")

            val result2 =
              journey.submitCorrectAmount(dutyType, taxCode, pa, BigDecimal("-0.01"))
            result2 shouldBe Left("submitAmountForReimbursement.invalidReimbursementAmount")

            val result3 =
              journey.submitCorrectAmount(dutyType, taxCode, pa, pa + BigDecimal("0.01"))
            result3 shouldBe Left("submitAmountForReimbursement.invalidReimbursementAmount")
          }
        }
      }
    }

    "submit inspection date" in {
      forAll(DateGen.genDate) { inspectionDate =>
        val journey = RejectedGoodsScheduledJourney.empty(exampleEori).submitInspectionDate(inspectionDate)

        journey.answers.inspectionDate shouldBe Some(inspectionDate)

      }
    }

    "change inspection date" in {
      forAll(completeJourneyGen, DateGen.genDate) { (journey, inspectionDate) =>
        val modifiedJourney = journey.submitInspectionDate(inspectionDate)

        modifiedJourney.hasCompleteAnswers             shouldBe true
        modifiedJourney.toOutput.map(_.inspectionDate) shouldBe Right(inspectionDate)

      }
    }

    "submit inspection address" in {
      forAll(InspectionAddressGen.genInspectionAddress) { inspectionAddress =>
        val journey = RejectedGoodsScheduledJourney.empty(exampleEori).submitInspectionAddress(inspectionAddress)

        journey.answers.inspectionAddress shouldBe Some(inspectionAddress)

      }
    }

    "change inspection address" in {
      forAll(completeJourneyGen, InspectionAddressGen.genInspectionAddress) { (journey, inspectionAddress) =>
        val modifiedJourney = journey.submitInspectionAddress(inspectionAddress)

        modifiedJourney.hasCompleteAnswers                shouldBe true
        modifiedJourney.toOutput.map(_.inspectionAddress) shouldBe Right(inspectionAddress)
      }
    }

    "submit bankAccountDetails and bankAccountType" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceDutyTypeSetForReimbursement(DutyTypes.custom))
          .flatMap(_.selectAndReplaceTaxCodeSetForDutyType(DutyType.UkDuty, Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(DutyType.UkDuty, TaxCode.A00, BigDecimal("2.00"), BigDecimal("1.00")))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.Business))

      journeyEither.getOrFail
    }

    "change bankAccountDetails in a complete journey with all duties CMA eligible" in {
      forAll(completeJourneyGen) { journey =>
        val journeyEither =
          journey.submitBankAccountDetails(exampleBankAccountDetails)

        journeyEither.isRight shouldBe journey.needsBanksAccountDetailsSubmission
      }
    }

    "hasCompleteReimbursementClaims" when {
      "return true if all claim amounts are present" in {
        forAll(completeJourneyGen) { journey =>
          journey.hasCompleteReimbursementClaims shouldBe true
        }
      }
    }

    "find next duty type - examples" in {
      val journey = RejectedGoodsScheduledJourney.empty(exampleEori)

      val dutyTypes       = Seq(DutyType.EuDuty, DutyType.Excise)
      val modifiedJourney =
        journey.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes).getOrFail

      modifiedJourney.findNextSelectedDutyAfter(DutyType.Excise) shouldBe None
      modifiedJourney.findNextSelectedDutyAfter(DutyType.EuDuty) shouldBe Some(DutyType.Excise)
    }

    "find next duty type" in {
      val journey = RejectedGoodsScheduledJourney.empty(exampleEori)
      forAll(dutyTypesGen) { dutyTypes =>
        val modifiedJourney =
          journey.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes).getOrFail

        val expected: Seq[(DutyType, DutyType)] = dutyTypes.init.zip(dutyTypes.tail)

        expected.foreach { case (previous, next) =>
          modifiedJourney.findNextSelectedDutyAfter(previous) shouldBe Some(next)
        }

        modifiedJourney.findNextSelectedDutyAfter(dutyTypes.last) shouldBe None
      }
    }

    "find next duty type and tax code pair" in {
      val journey = RejectedGoodsScheduledJourney.empty(exampleEori)
      forAll(dutyTypesWithTaxCodesGen) { (dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]) =>
        whenever(dutyTypesWithTaxCodes.nonEmpty && dutyTypesWithTaxCodes.forall(_._2.nonEmpty)) {

          val dutyTypes: Seq[DutyType] = dutyTypesWithTaxCodes.map(_._1)
          val modifiedJourney          =
            journey
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
            modifiedJourney.findNextSelectedTaxCodeAfter(
              previousDutyType,
              previousTaxCode
            ) shouldBe Some((nextDutyType, nextTaxCode))
          }

          val (lastDutyType, lastTaxCode) = {
            val (dt, tcs) = dutyTypesWithTaxCodes.last
            (dt, tcs.last)
          }

          modifiedJourney.findNextSelectedTaxCodeAfter(
            lastDutyType,
            lastTaxCode
          ) shouldBe None
        }
      }
    }

    "validate if any subsidy payment method is in the declaration" in {

      import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport.withSomeSubsidiesPaymentMethod

      val declaration =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
          .withSomeSubsidiesPaymentMethod()

      val journey = RejectedGoodsScheduledJourney
        .empty(
          exampleEori
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
        .getOrFail

      RejectedGoodsScheduledJourney.Checks.declarationsHasNoSubsidyPayments.apply(
        journey
      ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)
    }

  }
}
