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

import com.github.arturopala.validator.Validator
import org.scalacheck.Gen
import org.scalacheck.ShrinkLowPriority
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyValidationErrors.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*

import java.util.Locale

class OverpaymentsSingleJourneySpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "OverpaymentsSingleJourney" should {
    "have an empty instance" in {
      emptyJourney.answers.userEoriNumber                                         shouldBe exampleEori
      emptyJourney.answers.bankAccountDetails                                     shouldBe None
      emptyJourney.answers.bankAccountType                                        shouldBe None
      emptyJourney.answers.basisOfClaim                                           shouldBe None
      emptyJourney.answers.duplicateDeclaration                                   shouldBe None
      emptyJourney.answers.contactAddress                                         shouldBe None
      emptyJourney.answers.contactDetails                                         shouldBe None
      emptyJourney.answers.contactAddress                                         shouldBe None
      emptyJourney.answers.eoriNumbersVerification                                shouldBe None
      emptyJourney.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber) shouldBe None
      emptyJourney.answers.additionalDetails                                      shouldBe None
      emptyJourney.answers.displayDeclaration                                     shouldBe None
      emptyJourney.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyJourney.answers.correctedAmounts                                       shouldBe None
      emptyJourney.answers.reimbursementMethod                                    shouldBe None
      emptyJourney.answers.selectedDocumentType                                   shouldBe None
      emptyJourney.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyJourney.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyJourney.answers.dutiesChangeMode                                       shouldBe false
      emptyJourney.getNdrcDetails                                                 shouldBe None
      emptyJourney.getSelectedDuties                                              shouldBe None
      emptyJourney.isAllSelectedDutiesAreCMAEligible                              shouldBe false
      emptyJourney.hasCompleteReimbursementClaims                                 shouldBe false
      emptyJourney.hasCompleteSupportingEvidences                                 shouldBe false
      emptyJourney.hasCompleteAnswers                                             shouldBe false
      emptyJourney.toOutput.isLeft                                                shouldBe true
      emptyJourney.isFinalized                                                    shouldBe false
      emptyJourney.containsUnsupportedTaxCode                                     shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeJourneyGen) { journey =>
        OverpaymentsSingleJourney.validator.apply(journey) shouldBe Right(())
        journey.answers.checkYourAnswersChangeMode         shouldBe true
        journey.hasCompleteReimbursementClaims             shouldBe true
        journey.hasCompleteSupportingEvidences             shouldBe true
        journey.hasCompleteAnswers                         shouldBe true
        journey.isFinalized                                shouldBe false
        journey.containsUnsupportedTaxCode                 shouldBe false

        journey.getTotalReimbursementAmount shouldBe (
          journey.getEUDutyReimbursementTotal.getOrElse(ZERO) +
            journey.getUKDutyReimbursementTotal.getOrElse(ZERO) +
            journey.getExciseDutyReimbursementTotal.getOrElse(ZERO)
        )

        val output = journey.toOutput.getOrElse(fail("Journey output not defined."))

        output.movementReferenceNumber  shouldBe journey.answers.movementReferenceNumber.get
        output.claimantType             shouldBe journey.getClaimantType
        output.basisOfClaim             shouldBe journey.answers.basisOfClaim.get
        output.additionalDetails        shouldBe journey.answers.additionalDetails.get
        output.reimbursementMethod      shouldBe journey.answers.reimbursementMethod
          .getOrElse(ReimbursementMethod.BankAccountTransfer)
        output.reimbursements           shouldBe journey.getReimbursements
        output.supportingEvidences      shouldBe journey.answers.supportingEvidences.map(EvidenceDocument.from)
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

    "fail to finalize invalid journey" in {
      completeJourneyGen.sample.get
        .removeBankAccountDetails()
        .finalizeJourneyWith("foo")
        .isLeft shouldBe true
    }

    "have working equals method" in {
      val journey = completeJourneyGen.sample.get
      journey.equals(completeJourneyGen.sample.get) shouldBe false
      journey.equals(journey)                       shouldBe true
      journey.equals("foo")                         shouldBe false
      journey.hashCode()                            shouldBe journey.answers.hashCode
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
        OverpaymentsSingleJourney
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
        OverpaymentsSingleJourney
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

    "needs XI eori submission if user's eori not matching those of a duplicate ACC14, and duplicate ACC14 contains XI eori" in {
      val duplicateDisplayDeclaration =
        buildDisplayDeclaration(
          id = anotherExampleMrn.value,
          declarantEORI = anotherExampleEori,
          consigneeEORI = Some(exampleXIEori)
        )

      val journey =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(
            exampleMrn,
            exampleDisplayDeclaration
              .withDeclarationId(exampleMrn.value)
              .withDeclarantEori(exampleEori)
          )
          .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
          .flatMap(
            _.submitDuplicateMovementReferenceNumberAndDeclaration(
              anotherExampleMrn,
              duplicateDisplayDeclaration
            )
          )
          .getOrFail

      journey.userHasGBEoriMatchingDeclaration         shouldBe true
      journey.userHasXIEoriMatchingDeclaration         shouldBe false
      journey.needsUserXiEoriSubmission                shouldBe false
      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false

      journey.userHasGBEoriMatchingDuplicateDeclaration        shouldBe false
      journey.userHasXIEoriMatchingDuplicateDeclaration        shouldBe false
      journey.needsUserXiEoriSubmissionForDuplicateDeclaration shouldBe true

      val journey2 = journey.submitUserXiEori(UserXiEori(exampleXIEori.value))

      journey2.userHasGBEoriMatchingDeclaration         shouldBe true
      journey2.userHasXIEoriMatchingDeclaration         shouldBe false
      journey2.needsUserXiEoriSubmission                shouldBe false
      journey2.needsDeclarantAndConsigneeEoriSubmission shouldBe false

      journey2.userHasGBEoriMatchingDuplicateDeclaration        shouldBe false
      journey2.userHasXIEoriMatchingDuplicateDeclaration        shouldBe true
      journey2.needsUserXiEoriSubmissionForDuplicateDeclaration shouldBe false
    }

    "does not need declarant and consignee submission if user's eori is matching that of declarant" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori, consigneeEORI = None)
      val journey            =
        OverpaymentsSingleJourney
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
        OverpaymentsSingleJourney
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
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(Locale.ENGLISH))))
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Declarant
      journey.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is present" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleXIEori, consigneeEORI = Some(anotherExampleEori))
      val journey            =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(Locale.ENGLISH))))
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Declarant
      journey.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of consignee" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val journey            =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(Locale.ENGLISH))))
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
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "submit declarant eori number" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      journeyEither.getOrFail.answers.eoriNumbersVerification.get.declarantEoriNumber shouldBe Some(anotherExampleEori)
    }

    "fail if submitted declarant EORI is not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val journeyEither      =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val journey =
          OverpaymentsSingleJourney
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
        val journey = OverpaymentsSingleJourney.empty(exampleEori).submitContactAddress(contactAddress)

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
      forAll(Gen.oneOf(BasisOfOverpaymentClaim.values)) { basisOfClaim =>
        val journey = OverpaymentsSingleJourney.empty(exampleEori).submitBasisOfClaim(basisOfClaim)
        journey.answers.basisOfClaim shouldBe Some(basisOfClaim)
      }
    }

    "change basis of claim" in {
      forAll(
        completeJourneyGenWithoutDuplicateEntryAndIncorrectExciseValue,
        Gen.oneOf(
          BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.DuplicateEntry - BasisOfOverpaymentClaim.IncorrectExciseValue - BasisOfOverpaymentClaim.IncorrectEoriAndDan
        )
      ) { (journey, basisOfClaim) =>
        val modifiedJourney = journey.submitBasisOfClaim(basisOfClaim)

        modifiedJourney.hasCompleteAnswers           shouldBe true
        modifiedJourney.toOutput.map(_.basisOfClaim) shouldBe Right(basisOfClaim)
      }
    }

    "change basis of claim if duplicate entry" in {
      forAll(completeJourneyGenWithoutDuplicateEntryAndIncorrectExciseValue) { journey =>
        val modifiedJourney = journey.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry)

        modifiedJourney.hasCompleteAnswers           shouldBe false
        modifiedJourney.toOutput.map(_.basisOfClaim) shouldBe Left(
          DUPLICATE_MOVEMENT_REFERENCE_NUMBER_MUST_BE_DEFINED :: DUPLICATE_DISPLAY_DECLARATION_MUST_BE_DEFINED :: Nil
        )
      }
    }

    "change basis of claim if incorrect excise entry" in {
      forAll(completeJourneyGenWithoutDuplicateEntryAndIncorrectExciseValue) { journey =>
        val modifiedJourney = journey.submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectExciseValue)

        modifiedJourney.hasCompleteAnswers shouldBe modifiedJourney.hasCompleteReimbursementClaims
      }
    }

    "change basis of claim if IncorrectEoriAndDan" in {
      val basisOfClaim  = BasisOfOverpaymentClaim.IncorrectEoriAndDan
      val journeyEither = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitBasisOfClaim(basisOfClaim)
        .submitNewDan(exampleDan)
        .submitNewEori(exampleEori)

      val modifiedJourney = journeyEither.submitBasisOfClaim(basisOfClaim)

      modifiedJourney.answers.basisOfClaim shouldBe Some(basisOfClaim)
      modifiedJourney.answers.newEori      shouldBe Some(exampleEori)
      modifiedJourney.answers.newDan       shouldBe Some(exampleDan)
    }

    "submit duplicate mrn and declaration" in {
      val journeyEither = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
        .flatMap(
          _.submitDuplicateMovementReferenceNumberAndDeclaration(
            anotherExampleMrn,
            exampleDisplayDeclaration.withDeclarationId(anotherExampleMrn.value)
          )
        )

      journeyEither.isRight shouldBe true
    }

    "remove duplicated declaration when basis of claim changes" in {
      val journeyEither = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
        .flatMap(
          _.submitDuplicateMovementReferenceNumberAndDeclaration(
            anotherExampleMrn,
            exampleDisplayDeclaration.withDeclarationId(anotherExampleMrn.value)
          )
        )
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.EndUseRelief))

      journeyEither.isRight                                shouldBe true
      journeyEither.getOrFail.answers.duplicateDeclaration shouldBe None
    }

    "reject duplicate mrn and declaration if same mrn as main" in {
      val journeyEither = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
        .flatMap(
          _.submitDuplicateMovementReferenceNumberAndDeclaration(
            exampleMrn,
            exampleDisplayDeclaration
          )
        )

      journeyEither.isRight shouldBe false
    }

    "reject duplicate mrn and declaration if same declaration as main" in {
      val journeyEither = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
        .flatMap(
          _.submitDuplicateMovementReferenceNumberAndDeclaration(
            anotherExampleMrn,
            exampleDisplayDeclaration
          )
        )

      journeyEither.isRight shouldBe false
    }

    "change duplicate mrn and declaration" in {
      forAll(completeJourneyGenWithDuplicateEntry) { journey =>
        val duplicateMrn          = IdGen.genMRN.sample.get
        val modifiedJourneyEither =
          journey
            .submitDuplicateMovementReferenceNumberAndDeclaration(
              duplicateMrn,
              exampleDisplayDeclaration.withDeclarationId(duplicateMrn.value)
            )

        modifiedJourneyEither.isRight shouldBe journey.needsDuplicateMrnAndDeclaration
      }
    }

    "submit additional details" in {
      val journey = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitAdditionalDetails("foo bar")

      journey.answers.additionalDetails shouldBe Some("foo bar")
    }

    "change details of rejected goods" in {
      forAll(completeJourneyGen, Gen.asciiPrintableStr) { (journey, additionalDetails) =>
        val modifiedJourney = journey.submitAdditionalDetails(additionalDetails)

        modifiedJourney.hasCompleteAnswers                shouldBe true
        modifiedJourney.toOutput.map(_.additionalDetails) shouldBe Right(additionalDetails)
      }
    }

    "select valid tax codes for reimbursement when none yet selected" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00, TaxCode.A90)))

      journeyEither.isRight                    shouldBe true
      journeyEither.getOrFail.getAvailableDuties should contain theSameElementsAs Seq(
        (TaxCode.A00, false),
        (TaxCode.A90, false)
      )
    }

    "return left when selecting valid tax codes for reimbursement without a display declaration" in {
      val journeyEither = OverpaymentsSingleJourney
        .empty(exampleEori)
        .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00, TaxCode.A90))

      journeyEither shouldBe Left("selectTaxCodeSetForReimbursement.missingDisplayDeclaration")
    }

    "return left when selecting empty list of tax codes for reimbursement" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq.empty))

      journeyEither shouldBe Left("selectTaxCodeSetForReimbursement.emptySelection")
    }

    "select valid tax codes for reimbursement when none yet selected with excise code in declaration" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.NI407, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectExciseValue))
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.NI407)))

      journeyEither.isRight                    shouldBe true
      journeyEither.getOrFail.getAvailableDuties should contain theSameElementsAs Seq((TaxCode.NI407, false))
    }

    "return all available claim types when excise code in declaration" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.NI407, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)

      journeyEither.isRight                        shouldBe true
      journeyEither.getOrFail.getAvailableClaimTypes should contain theSameElementsAs BasisOfOverpaymentClaim.values
    }

    "return all available claim types except IncorrectExciseValue when no excise code in declaration" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)

      journeyEither.isRight                        shouldBe true
      journeyEither.getOrFail.getAvailableClaimTypes should contain theSameElementsAs (BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.IncorrectExciseValue)
    }

    "replace valid tax codes for reimbursement" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))

      journeyEither.getOrFail.getSelectedDuties shouldBe Some(Seq(TaxCode.A00))

      val journeyEither2 =
        journeyEither.flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A90, TaxCode.A20)))

      journeyEither2.getOrFail.getSelectedDuties shouldBe Some(
        Seq(TaxCode.A90, TaxCode.A20)
      )

    }

    "replace valid tax codes for reimbursement with non-CMA duty addition when CurrentMonthAdjustment" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A20)))
        .flatMap(_.submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment))

      journeyEither.getOrFail.getSelectedDuties           shouldBe Some(Seq(TaxCode.A20))
      journeyEither.getOrFail.answers.reimbursementMethod shouldBe Some(ReimbursementMethod.CurrentMonthAdjustment)

      val journeyEither2 =
        journeyEither.flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A20, TaxCode.A00)))

      journeyEither2.getOrFail.getSelectedDuties           shouldBe Some(
        Seq(TaxCode.A20, TaxCode.A00)
      )
      journeyEither2.getOrFail.answers.reimbursementMethod shouldBe None
    }

    "replace valid tax codes for reimbursement with non-CMA duty addition when BankAccountTransfer" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A20)))
        .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))

      journeyEither.getOrFail.getSelectedDuties           shouldBe Some(Seq(TaxCode.A20))
      journeyEither.getOrFail.answers.reimbursementMethod shouldBe Some(ReimbursementMethod.BankAccountTransfer)

      val journeyEither2 =
        journeyEither.flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A20, TaxCode.A00)))

      journeyEither2.getOrFail.getSelectedDuties           shouldBe Some(
        Seq(TaxCode.A20, TaxCode.A00)
      )
      journeyEither2.getOrFail.answers.reimbursementMethod shouldBe None
    }

    "select invalid tax codes for reimbursement" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("1.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A80)))

      journeyEither.isRight shouldBe false
    }

    "change tax code for reimbursement with the same set" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourneyEither = journey.selectAndReplaceTaxCodeSetForReimbursement(journey.getSelectedDuties.get)

        val result = modifiedJourneyEither.getOrFail
        result.hasCompleteAnswers shouldBe true
      }
    }

    "change tax code for reimbursement with a new valid set" in {
      forAll(completeJourneyGen) { journey =>
        val taxCodeSet                  = journey.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val newTaxCodeSet: Seq[TaxCode] = taxCodeSet.take(2).toSeq

        val modifiedJourneyEither = journey.selectAndReplaceTaxCodeSetForReimbursement(newTaxCodeSet)

        val result = modifiedJourneyEither.getOrFail
        result.getSelectedDuties.get shouldBe newTaxCodeSet
      }
    }

    "change tax code for reimbursement with a new invalid set" in {
      forAll(completeJourneyGen) { journey =>
        val invalidTaxCodeSet     = TaxCodes.all.take(6)
        val modifiedJourneyEither = journey.selectAndReplaceTaxCodeSetForReimbursement(invalidTaxCodeSet)
        modifiedJourneyEither shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
      }
    }

    "submit valid correct amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("5.00"))))

      journeyEither.isRight shouldBe true
    }

    "submit valid claim amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("6.66")))

      journeyEither.isRight                               shouldBe true
      journeyEither.getOrFail.getTotalReimbursementAmount shouldBe BigDecimal("6.66")
    }

    "submit valid correct amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A80, DefaultMethodReimbursementClaim(BigDecimal("5.00"))))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "submit valid claim amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(TaxCode.A80, BigDecimal("6.66")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "return left when submitting valid correct amount with missing display declaration" in {
      val journeyEither = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitCorrectAmount(TaxCode.A80, DefaultMethodReimbursementClaim(BigDecimal("5.00")))

      journeyEither shouldBe Left("submitCorrectAmount.missingDisplayDeclaration")
    }

    "return left when submitting valid claim amount with missing display declaration" in {
      val journeyEither = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitClaimAmount(TaxCode.A80, BigDecimal("6.66"))

      journeyEither shouldBe Left("submitCorrectAmount.missingDisplayDeclaration")
    }

    "return left when submitting valid correct amount for with no tax code selected" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("5.00"))))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotSelectedYet")
    }

    "return left when submitting valid claim amount for with no tax code selected" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("6.66")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotSelectedYet")
    }

    "submit invalid correct amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration        = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))

      val journeyEitherTestZero     =
        declaration.flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("10.00"))))
      val journeyEitherTestNegative =
        declaration.flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("-10.00"))))
      val journeyEitherTestGreater  =
        declaration.flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("20.00"))))

      journeyEitherTestZero     shouldBe Left("submitCorrectAmount.invalidAmount")
      journeyEitherTestNegative shouldBe Left("submitCorrectAmount.invalidAmount")
      journeyEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidAmount")
    }

    "submit invalid claim amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration        = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))

      val journeyEitherTestZero     =
        declaration.flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("0")))
      val journeyEitherTestNegative =
        declaration.flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("-0.01")))
      val journeyEitherTestGreater  =
        declaration.flatMap(_.submitClaimAmount(TaxCode.A00, BigDecimal("10.01")))

      journeyEitherTestZero     shouldBe Left("submitCorrectAmount.invalidAmount")
      journeyEitherTestNegative shouldBe Left("submitCorrectAmount.invalidAmount")
      journeyEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidAmount")
    }

    "submit invalid correct amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A80, DefaultMethodReimbursementClaim(BigDecimal("10.0"))))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "submit invalid claim amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(TaxCode.A80, BigDecimal("0.00")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "change to valid amount for selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        val totalAmount: BigDecimal              = journey.getTotalReimbursementAmount
        val taxCodes: Seq[(TaxCode, BigDecimal)] = journey.getSelectedTaxCodesWithCorrectAmount
        for (taxCode, correctAmount) <- taxCodes do {
          val newCorrectAmount = correctAmount / 2
          val journeyEither    = journey.submitCorrectAmount(taxCode, DefaultMethodReimbursementClaim(newCorrectAmount))
          journeyEither.isRight shouldBe true
          val modifiedJourney = journeyEither.getOrFail
          modifiedJourney.getTotalReimbursementAmount shouldBe (totalAmount + newCorrectAmount)
        }
      }
    }

    "change to invalid amount for selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        val taxCodes: Seq[TaxCode] = journey.getSelectedDuties.get

        for taxCode <- taxCodes do {
          val ndrcDetails   = journey.getNdrcDetailsFor(taxCode).get
          val newAmount     = BigDecimal(ndrcDetails.amount)
          val journeyEither = journey.submitCorrectAmount(taxCode, DefaultMethodReimbursementClaim(newAmount))

          journeyEither shouldBe Left("submitCorrectAmount.invalidAmount")
        }
      }
    }

    "change to valid amount for the tax code not in ACC14" in {
      forAll(completeJourneyGen) { journey =>
        val taxCodeSet    = journey.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val wrongTaxCode  = TaxCodes.all.find(taxCode => !taxCodeSet.contains(taxCode)).getOrElse(TaxCode.NI633)
        val journeyEither =
          journey.submitCorrectAmount(wrongTaxCode, DefaultMethodReimbursementClaim(BigDecimal("10.00")))
        journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
      }
    }

    "submit CurrentMonthAdjustment as reimbursement method when all duties are CMA eligible" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("0.99"))))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment))

      journeyEither.isRight shouldBe true
    }

    "fail submitting CurrentMonthAdjustment as reimbursement method when NOT all duties are CMA eligible" in {
      val displayDeclarationNotCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), false)))
      val journeyEither                    =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationNotCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("0.00"))))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment))

      journeyEither shouldBe Left("submitReimbursementMethod.notCMAEligible")
    }

    "submit BankAccountTransfer as reimbursement method when all duties are CMA eligible" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("0.01"))))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))

      journeyEither.isRight shouldBe true
    }

    "submit BankAccountTransfer as reimbursement method when NOT all duties are CMA eligible" in {
      val displayDeclarationNotCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), false)))
      val journeyEither                    =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationNotCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("0.00"))))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))

      journeyEither.isRight shouldBe true
    }

    "computeBankDetails" should {
      "return consigneeBankDetails when payeeType is consignee" in {
        val declaration = buildDisplayDeclaration(
          dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)),
          consigneeBankDetails = Some(exampleConsigneeBankAccountDetails),
          declarantBankDetails = Some(exampleDeclarantBankAccountDetails)
        )

        val journeyEither =
          OverpaymentsSingleJourney
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
            .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
            .flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("0.00"))))
            .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))
            .flatMap(_.submitPayeeType(PayeeType.Consignee))

        journeyEither.isRight                                shouldBe true
        journeyEither.toOption.get.computeBankAccountDetails shouldBe Some(exampleConsigneeBankAccountDetails)
      }

      "return declarantBankDetails when payeeType is declarant" in {
        val declaration = buildDisplayDeclaration(
          dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)),
          consigneeBankDetails = Some(exampleConsigneeBankAccountDetails),
          declarantBankDetails = Some(exampleDeclarantBankAccountDetails)
        )

        val journeyEither =
          OverpaymentsSingleJourney
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
            .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
            .flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("0.00"))))
            .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))
            .flatMap(_.submitPayeeType(PayeeType.Declarant))

        journeyEither.isRight                                                shouldBe true
        journeyEither.toOption.get.computeBankAccountDetails                 shouldBe Some(exampleDeclarantBankAccountDetails)
        journeyEither.getOrFail.submitPayeeType(PayeeType.Declarant).isRight shouldBe true
      }
    }

    "submit bankAccountDetails and bankAccountType if reimbursement method is BankAccountTransfer" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, DefaultMethodReimbursementClaim(BigDecimal("0.00"))))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.Business))

      journeyEither.isRight shouldBe true
    }

    "change reimbursementMethod to CMA in a complete journey with all duties CMA eligible" in {
      forAll(completeJourneyCMAEligibleGen) { journey =>
        whenever(journey.needsBanksAccountDetailsSubmission) {
          val modifiedJourney =
            journey
              .submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment)
              .getOrFail

          modifiedJourney.hasCompleteAnswers shouldBe true
        }
      }
    }

    "change bankAccountDetails in a complete journey with all duties CMA eligible" in {
      forAll(completeJourneyCMAEligibleGen) { journey =>
        val journeyEither =
          journey.submitBankAccountDetails(exampleBankAccountDetails)

        journeyEither.isRight shouldBe journey.needsBanksAccountDetailsSubmission
      }
    }

    "change bankAccountDetails in a complete journey not eligible for CMA" in {
      forAll(completeJourneyNotCMAEligibleGen) { journey =>
        val journeyEither =
          journey.submitBankAccountDetails(exampleBankAccountDetails)

        journeyEither.isRight shouldBe true
      }
    }

    "hasCompleteReimbursementClaims" when {
      "return true if all claim amounts are present" in {
        forAll(completeJourneyGen) { journey =>
          journey.hasCompleteReimbursementClaims shouldBe true
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
              val taxCodes       = ndrcDetails.map(details => TaxCode(details.taxType))
              val drd            = displayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
              val updatedDd      = displayDeclaration.copy(displayResponseDetail = drd)
              val initialJourney = OverpaymentsSingleJourney
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(exampleMrn, updatedDd)
                .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(taxCodes))
                .getOrFail
              val journeyToTest  = ndrcDetails.dropRight(1).foldLeft(initialJourney) { case (journey, ndrcDetails) =>
                journey.submitCorrectAmount(TaxCode(ndrcDetails.taxType), DefaultMethodReimbursementClaim(1)).getOrFail
              }
              journeyToTest.hasCompleteReimbursementClaims shouldBe false
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
              val journey   = OverpaymentsSingleJourney
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(exampleMrn, updatedDd)
                .getOrFail

              journey.hasCompleteReimbursementClaims shouldBe false
            }
        }
      }
    }

    "validate if any subsidy payment method is in the declaration" when {

      import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport

      "BlockSubsidies feature not enabled" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe None

        OverpaymentsSingleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Valid
      }

      "BlockSubsidies feature enabled and SubsidyOnlyPayments not" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = OverpaymentsSingleJourney
          .empty(
            exampleEori,
            features = Some(
              OverpaymentsSingleJourney
                .Features(
                  shouldBlockSubsidies = true,
                  shouldAllowSubsidyOnlyPayments = false,
                  shouldSkipDocumentTypeSelection = false
                )
            )
          )
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe Some(
          OverpaymentsSingleJourney.Features(
            shouldBlockSubsidies = true,
            shouldAllowSubsidyOnlyPayments = false,
            shouldSkipDocumentTypeSelection = false
          )
        )

        OverpaymentsSingleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)
      }

      "BlockSubsidies feature disabled and SubsidyOnlyPayments enabled" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = OverpaymentsSingleJourney
          .empty(
            exampleEori,
            features = Some(
              OverpaymentsSingleJourney
                .Features(
                  shouldBlockSubsidies = false,
                  shouldAllowSubsidyOnlyPayments = true,
                  shouldSkipDocumentTypeSelection = false
                )
            )
          )
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe Some(
          OverpaymentsSingleJourney.Features(
            shouldBlockSubsidies = false,
            shouldAllowSubsidyOnlyPayments = true,
            shouldSkipDocumentTypeSelection = false
          )
        )

        OverpaymentsSingleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Valid
      }

      "both BlockSubsidies and SubsidyOnlyPayments features enabled" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = OverpaymentsSingleJourney
          .empty(
            exampleEori,
            features = Some(
              OverpaymentsSingleJourney.Features(
                shouldBlockSubsidies = true,
                shouldAllowSubsidyOnlyPayments = true,
                shouldSkipDocumentTypeSelection = false
              )
            )
          )
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe Some(
          OverpaymentsSingleJourney.Features(
            shouldBlockSubsidies = true,
            shouldAllowSubsidyOnlyPayments = true,
            shouldSkipDocumentTypeSelection = false
          )
        )

        OverpaymentsSingleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Valid
      }
    }

    "complete journey has new eori and dan when basis of claim is IncorrectEoriAndDan" in {
      forAll(completeJourneyCMAEligibleGen) { journey =>
        val modifiedJourney = journey
          .submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectEoriAndDan)
          .submitNewEori(exampleEori)
          .submitNewDan(exampleDan)

        modifiedJourney.hasCompleteAnswers shouldBe true
      }
    }

    "return MISSING_NEW_EORI when complete journey basis of claim is IncorrectEoriAndDan and new eori is not set" in {
      forAll(
        completeJourneyGen.suchThat(j => !j.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.IncorrectEoriAndDan))
      ) { journey =>
        val modifiedJourney = journey
          .submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectEoriAndDan)
          .submitNewDan(exampleDan)

        OverpaymentsSingleJourney.Checks.newEoriAndDanProvidedIfNeeded.apply(
          modifiedJourney
        ) shouldBe Validator.Invalid(MISSING_NEW_EORI)
      }
    }

    "return MISSING_NEW_DAN when complete journey basis of claim is IncorrectEoriAndDan and new dan is not set" in {
      forAll(
        completeJourneyGen.suchThat(j => !j.answers.basisOfClaim.contains(BasisOfOverpaymentClaim.IncorrectEoriAndDan))
      ) { journey =>
        val modifiedJourney = journey
          .submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectEoriAndDan)
          .submitNewEori(exampleEori)

        OverpaymentsSingleJourney.Checks.newEoriAndDanProvidedIfNeeded.apply(
          modifiedJourney
        ) shouldBe Validator.Invalid(MISSING_NEW_DAN)
      }
    }

    "submit document type selection" in {
      val documentType = UploadDocumentType.overpaymentsSingleDocumentTypes(0)
      val journey      =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitDocumentTypeSelection(documentType)

      journey.answers.selectedDocumentType shouldBe Some(documentType)
    }
  }

  "change document type selection" in {
    val newDocumentType = UploadDocumentType.overpaymentsSingleDocumentTypes(1)
    val journey         =
      OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitDocumentTypeSelection(UploadDocumentType.overpaymentsSingleDocumentTypes.head)

    val modifiedJourney = journey.submitDocumentTypeSelection(newDocumentType)

    modifiedJourney.answers.selectedDocumentType shouldBe Some(newDocumentType)
  }

  "reset reimbursement method" in {
    val journeyEither = OverpaymentsSingleJourney
      .empty(exampleEori)
      .submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer)
      .flatMap(_.submitBankAccountType(BankAccountType.Business))
      .flatMap(_.submitBankAccountDetails(BankAccountGen.genBankAccountDetails.sample.get))
      .getOrFail
      .resetReimbursementMethod()

    journeyEither.answers.reimbursementMethod shouldBe None
    journeyEither.answers.bankAccountType     shouldBe None
    journeyEither.answers.bankAccountDetails  shouldBe None
  }

  "remove bank account details" in {
    val journeyEither = OverpaymentsSingleJourney
      .empty(exampleEori)
      .submitBankAccountDetails(BankAccountGen.genBankAccountDetails.sample.get)
      .getOrFail
      .removeBankAccountDetails()

    journeyEither.answers.bankAccountDetails shouldBe None
  }

  "remove unsupported tax codes" in {
    val journey = OverpaymentsSingleJourney
      .empty(exampleEori)
      .submitMovementReferenceNumberAndDeclaration(
        exampleMrn,
        exampleDisplayDeclarationWithSomeUnsupportedCode
      )
      .getOrFail
      .removeUnsupportedTaxCodes()

    journey.containsUnsupportedTaxCode shouldBe false
  }

  "return left checking consignee eori with duplicate declaration when consignee eori doesn't match declaration eori" in {
    val displayDeclaration          = buildDisplayDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    )
    val duplicateDisplayDeclaration = buildDisplayDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    ).optionallyWithMRN(Some(anotherExampleMrn))
      .withConsigneeEori(anotherExampleEori)
      .withDeclarantEori(yetAnotherExampleEori)
    val journeyEither               = OverpaymentsSingleJourney
      .empty(exampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
      .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
      .flatMap(_.submitDuplicateMovementReferenceNumberAndDeclaration(anotherExampleMrn, duplicateDisplayDeclaration))

    journeyEither.getOrFail.checkConsigneeEoriNumberWithDuplicateDeclaration(exampleEori) shouldBe Left(
      "checkConsigneeEoriNumberWithDuplicateDeclaration.shouldMatchConsigneeEoriFromACC14"
    )
  }

  "return left checking declarant eori with duplicate declaration when consignee eori doesn't match declaration eori" in {
    val displayDeclaration          = buildDisplayDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    )
    val duplicateDisplayDeclaration = buildDisplayDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    ).optionallyWithMRN(Some(anotherExampleMrn))
      .withConsigneeEori(anotherExampleEori)
      .withDeclarantEori(yetAnotherExampleEori)
    val journeyEither               = OverpaymentsSingleJourney
      .empty(exampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
      .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
      .flatMap(_.submitDuplicateMovementReferenceNumberAndDeclaration(anotherExampleMrn, duplicateDisplayDeclaration))

    journeyEither.getOrFail.checkDeclarantEoriNumberWithDuplicateDeclaration(exampleEori) shouldBe Left(
      JourneyValidationErrors.SHOULD_MATCH_ACC14_DUPLICATE_DECLARANT_EORI
    )
  }

  "successfully accept declarant eori with duplicate declaration when no previous consignee check" in {
    val displayDeclaration          = buildDisplayDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    )
    val duplicateDisplayDeclaration = buildDisplayDeclaration(dutyDetails =
      Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
    ).optionallyWithMRN(Some(anotherExampleMrn))
      .withConsigneeEori(anotherExampleEori)
      .withDeclarantEori(yetAnotherExampleEori)
    val journeyEither               = OverpaymentsSingleJourney
      .empty(exampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
      .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
      .flatMap(_.submitDuplicateMovementReferenceNumberAndDeclaration(anotherExampleMrn, duplicateDisplayDeclaration))

    journeyEither.getOrFail
      .checkDeclarantEoriNumberWithDuplicateDeclaration(yetAnotherExampleEori)
      .isRight shouldBe true
  }

  "receive uploaded files" in {
    val journey      = completeJourneyGen.sample.get
    val uploadedFile = exampleUploadedFile

    val modifiedJourney = journey
      .receiveUploadedFiles(None, journey.answers.nonce, Seq(uploadedFile))

    modifiedJourney.isRight                             shouldBe true
    modifiedJourney.getOrFail.answers.supportingEvidences should contain theSameElementsAs Seq(uploadedFile)
  }

  "receive uploaded files and set document type if not set" in {
    val journey      = completeJourneyGen.sample.get
    val uploadedFile = exampleUploadedFile.copy(cargo = None)
    val newFileType  = UploadDocumentType.overpaymentsSingleDocumentTypes.head

    val modifiedJourney = journey
      .receiveUploadedFiles(Some(newFileType), journey.answers.nonce, Seq(uploadedFile))

    modifiedJourney.getOrFail.answers.supportingEvidences.head.cargo shouldBe Some(newFileType)
  }

  "return left when receiving uploaded files with invalid nonce" in {
    val journey      = completeJourneyGen.sample.get
    val uploadedFile = exampleUploadedFile

    journey.receiveUploadedFiles(None, Nonce.random, Seq(uploadedFile)) shouldBe Left(
      "receiveUploadedFiles.invalidNonce"
    )
  }
}
