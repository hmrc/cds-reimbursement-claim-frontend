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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*

class OverpaymentsMultipleJourneySpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "OverpaymentsMultipleJourney" should {
    "have an empty instance" in {
      emptyJourney.answers.userEoriNumber                                         shouldBe exampleEori
      emptyJourney.answers.bankAccountDetails                                     shouldBe None
      emptyJourney.answers.bankAccountType                                        shouldBe None
      emptyJourney.answers.basisOfClaim                                           shouldBe None
      emptyJourney.answers.contactAddress                                         shouldBe None
      emptyJourney.answers.contactDetails                                         shouldBe None
      emptyJourney.answers.contactAddress                                         shouldBe None
      emptyJourney.answers.eoriNumbersVerification                                shouldBe None
      emptyJourney.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber) shouldBe None
      emptyJourney.answers.additionalDetails                                      shouldBe None
      emptyJourney.answers.displayDeclarations                                    shouldBe None
      emptyJourney.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyJourney.answers.correctedAmounts                                       shouldBe None
      emptyJourney.answers.selectedDocumentType                                   shouldBe None
      emptyJourney.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyJourney.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyJourney.getNdrcDetails                                                 shouldBe None
      emptyJourney.isAllSelectedDutiesAreCMAEligible                              shouldBe false
      emptyJourney.hasCompleteReimbursementClaims                                 shouldBe false
      emptyJourney.hasCompleteSupportingEvidences                                 shouldBe false
      emptyJourney.hasCompleteAnswers                                             shouldBe false
      emptyJourney.toOutput.isLeft                                                shouldBe true
      emptyJourney.isFinalized                                                    shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeJourneyGen) { journey =>
        OverpaymentsMultipleJourney.validator.apply(journey) shouldBe Right(())
        journey.answers.checkYourAnswersChangeMode           shouldBe true
        journey.hasCompleteReimbursementClaims               shouldBe true
        journey.hasCompleteSupportingEvidences               shouldBe true
        journey.hasCompleteAnswers                           shouldBe true
        journey.isFinalized                                  shouldBe false

        val output = journey.toOutput.getOrElse(fail("Journey output not defined."))

        output.movementReferenceNumbers shouldBe journey.answers.movementReferenceNumbers.get
        output.claimantType             shouldBe journey.getClaimantType
        output.basisOfClaim             shouldBe journey.answers.basisOfClaim.get
        output.additionalDetails        shouldBe journey.answers.additionalDetails.get
        output.reimbursementMethod      shouldBe ReimbursementMethod.BankAccountTransfer
        output.supportingEvidences      shouldBe journey.answers.supportingEvidences.map(EvidenceDocument.from)
        output.bankAccountDetails       shouldBe journey.answers.bankAccountDetails
        output.claimantInformation.eori shouldBe journey.answers.userEoriNumber
        output.reimbursementClaims      shouldBe journey.getReimbursementClaims
        output.reimbursementClaims.size shouldBe journey.countOfMovementReferenceNumbers
      }
    }

    "check incompleteness if less than two MRNs" in {
      forAll(buildCompleteJourneyGen(minNumberOfMRNs = 1, maxNumberOfMRNs = 1)) { journey =>
        OverpaymentsMultipleJourney.validator(journey).headErrorOption shouldBe Some(
          MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER
        )
        journey.answers.checkYourAnswersChangeMode                     shouldBe false
        journey.hasCompleteReimbursementClaims                         shouldBe true
        journey.hasCompleteSupportingEvidences                         shouldBe true
        journey.hasCompleteAnswers                                     shouldBe false
        journey.isFinalized                                            shouldBe false
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
        journey.answers.movementReferenceNumbers.flatMap(_.headOption).contains(mrn) shouldBe true
        journey.hasCompleteAnswers                                                   shouldBe false
        journey.hasCompleteReimbursementClaims                                       shouldBe false
        journey.hasCompleteSupportingEvidences                                       shouldBe false
        journey.isFinalized                                                          shouldBe false
      }
    }

    "decline submission of a wrong display declaration mrn" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, decl) =>
        val journeyEither = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(mrn, decl.withDeclarationId("foo"))

        journeyEither shouldBe Left("submitMovementReferenceNumber.wrongDisplayDeclarationMrn")
      }
    }

    "decline submission of a wrong display declaration eori" in {
      val displayDeclaration  =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val displayDeclaration2 =
        buildDisplayDeclaration(declarantEORI = exampleEori, consigneeEORI = Some(exampleEori))
      val journeyEither       =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, exampleMrn, displayDeclaration2)

      journeyEither shouldBe Left("submitMovementReferenceNumber.wrongDisplayDeclarationEori")
    }

    "decline submission of a display declaration with an already existing MRN" in {
      val displayDeclaration  =
        buildDisplayDeclaration(id = exampleMrnAsString)
      val displayDeclaration2 =
        buildDisplayDeclaration(id = exampleMrnAsString)
      val journeyEither       =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, exampleMrn, displayDeclaration2)

      journeyEither shouldBe Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
    }

    "decline submission of declaration at a negative index" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, decl) =>
        val journey = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(-1, mrn, decl)

        journey shouldBe Left("submitMovementReferenceNumber.negativeIndex")
      }
    }

    "decline submission of declaration at an invalid index" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, decl) =>
        val journey = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(1, mrn, decl)

        journey shouldBe Left("submitMovementReferenceNumber.invalidIndex")
      }
    }

    "accept change of the MRN" in {
      forAll(completeJourneyGen, displayDeclarationGen) { (journey, decl) =>
        val decl2           = decl.withDeclarationId(exampleMrnAsString)
        val modifiedJourney = journey
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, decl2)
          .getOrFail
        modifiedJourney.answers.displayDeclarations.flatMap(_.headOption) shouldBe Some(decl2)
        modifiedJourney.hasCompleteAnswers                                shouldBe false
        modifiedJourney.hasCompleteReimbursementClaims                    shouldBe false
        modifiedJourney.hasCompleteSupportingEvidences                    shouldBe false
      }
    }

    "decline change of the MRN if it already exists at a different index" in {
      val displayDeclaration  =
        buildDisplayDeclaration(id = exampleMrnAsString)
      val displayDeclaration2 =
        buildDisplayDeclaration(id = anotherExampleMrn.value)
      val journeyEither       =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, anotherExampleMrn, displayDeclaration2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(0, anotherExampleMrn, displayDeclaration2)

      journeyEither shouldBe Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
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
        modifiedJourney.getLeadDisplayDeclaration       shouldBe Some(decl2)
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
            journey.getLeadMovementReferenceNumber.get,
            journey.getLeadDisplayDeclaration.get
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

        journey.answers.movementReferenceNumbers.toList.flatten.contains(exampleMrn) shouldBe true
        journey.answers.displayDeclarations.toList.flatten
          .contains(acc14.withDeclarationId(exampleMrnAsString))                     shouldBe true
        journey.hasCompleteAnswers                                                   shouldBe false
        journey.hasCompleteReimbursementClaims                                       shouldBe false
        journey.hasCompleteSupportingEvidences                                       shouldBe false
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
        modifiedJourney.answers.movementReferenceNumbers shouldBe Some(List(exampleMrn))
        modifiedJourney.answers.displayDeclarations      shouldBe Some(List(exampleDisplayDeclaration))
        modifiedJourney.answers.correctedAmounts         shouldBe None
        modifiedJourney.hasCompleteAnswers               shouldBe false
        modifiedJourney.hasCompleteReimbursementClaims   shouldBe false
        modifiedJourney.hasCompleteSupportingEvidences   shouldBe false
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val journey            =
        OverpaymentsMultipleJourney
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
        OverpaymentsMultipleJourney
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
        OverpaymentsMultipleJourney
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
        OverpaymentsMultipleJourney
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
        OverpaymentsMultipleJourney
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
        OverpaymentsMultipleJourney
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
        OverpaymentsMultipleJourney
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
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "fail if submitted declarant EORI is not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val journeyEither      =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val journey =
          OverpaymentsMultipleJourney
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
        val journey = OverpaymentsMultipleJourney.empty(exampleEori).submitContactAddress(contactAddress)

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
        val journey = OverpaymentsMultipleJourney.empty(exampleEori).submitBasisOfClaim(basisOfClaim)
        journey.answers.basisOfClaim shouldBe Some(basisOfClaim)
      }
    }

    "submit additional details" in {
      val journey = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitAdditionalDetails("foo bar")

      journey.answers.additionalDetails shouldBe Some("foo bar")
    }

    "change details of rejected goods" in {
      forAll(completeJourneyGen, Gen.asciiPrintableStr) { (journey, additionalDetails) =>
        val modifiedJourney = journey.submitAdditionalDetails(additionalDetails)

        modifiedJourney.hasCompleteAnswers shouldBe true

        journey.answers.payeeType                         shouldBe Some(PayeeType.Declarant)
        modifiedJourney.answers.payeeType                 shouldBe Some(PayeeType.Declarant)
        modifiedJourney.toOutput.map(_.additionalDetails) shouldBe Right(additionalDetails)
      }
    }

    "select valid tax codes for reimbursement when none yet selected" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00, TaxCode.A90)))

      journeyEither.isRight                                                       shouldBe true
      journeyEither.getOrFail.getAmountPaidForIfSelected(exampleMrn, TaxCode.A00) shouldBe Some(BigDecimal("10.00"))
      journeyEither.getOrFail.getAmountPaidForIfSelected(exampleMrn, TaxCode.A20) shouldBe None
    }

    "return left when selecting valid tax codes for reimbursement without a display declaration" in {
      val journeyEither = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00, TaxCode.A90))

      journeyEither shouldBe Left("selectTaxCodeSetForReimbursement.missingDisplayDeclaration")
    }

    "return left when selecting empty list of tax codes for reimbursement" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq.empty))

      journeyEither shouldBe Left("selectTaxCodeSetForReimbursement.emptySelection")
    }

    "replace valid tax codes for reimbursement" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq(
          (TaxCode.A00, BigDecimal("10.00"), false),
          (TaxCode.A90, BigDecimal("20.00"), false),
          (TaxCode.A20, BigDecimal("30.00"), true)
        )
      )
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))

      journeyEither.getOrFail.getSelectedDuties(exampleMrn) shouldBe Some(Seq(TaxCode.A00))

      val journeyEither2 =
        journeyEither.flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A90, TaxCode.A20)))

      journeyEither2.getOrFail.getSelectedDuties(exampleMrn) shouldBe Some(
        Seq(TaxCode.A90, TaxCode.A20)
      )

    }

    "select invalid tax codes for reimbursement" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("1.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A80)))

      journeyEither.isRight shouldBe false
    }

    "change tax code for reimbursement with the same set" in {
      forAll(completeJourneyGen) { journey =>
        val mrn = journey.answers.movementReferenceNumbers.get.head

        val modifiedJourneyEither = journey.selectAndReplaceTaxCodeSetForReimbursement(
          mrn,
          journey.getSelectedDuties(mrn).get
        )

        val result = modifiedJourneyEither.getOrFail
        result.hasCompleteAnswers shouldBe true
      }
    }

    "change tax code for reimbursement with a new valid set" in {
      forAll(completeJourneyGen) { journey =>
        val taxCodeSet                  = journey.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val newTaxCodeSet: Seq[TaxCode] = taxCodeSet.take(2).toSeq
        val mrn                         = journey.answers.movementReferenceNumbers.get.head

        val modifiedJourneyEither = journey.selectAndReplaceTaxCodeSetForReimbursement(mrn, newTaxCodeSet)

        val result = modifiedJourneyEither.getOrFail
        result.getSelectedDuties(mrn).get shouldBe newTaxCodeSet
      }
    }

    "change tax code for reimbursement with a new invalid set" in {
      forAll(completeJourneyGen) { journey =>
        val invalidTaxCodeSet     = TaxCodes.all.take(6)
        val mrn                   = journey.answers.movementReferenceNumbers.get.head
        val modifiedJourneyEither = journey.selectAndReplaceTaxCodeSetForReimbursement(mrn, invalidTaxCodeSet)
        modifiedJourneyEither shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
      }
    }

    "submit valid correct amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("5.00")))

      journeyEither.isRight shouldBe true
    }

    "submit valid correct amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A80, BigDecimal("5.00")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "return left when submitting valid correct amount with missing display declaration" in {
      val journeyEither = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitCorrectAmount(exampleMrn, TaxCode.A80, BigDecimal("5.00"))

      journeyEither shouldBe Left("submitCorrectAmount.missingDisplayDeclaration")
    }

    "return left when submitting valid correct amount for with no tax code selected" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("5.00")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotSelectedYet")
    }

    "submit invalid correct amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration        = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))

      val journeyEitherTestZero     =
        declaration.flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("10.00")))
      val journeyEitherTestNegative =
        declaration.flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("-10.00")))
      val journeyEitherTestGreater  =
        declaration.flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("20.00")))

      journeyEitherTestZero     shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
      journeyEitherTestNegative shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
      journeyEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
    }

    "submit invalid correct amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A80, BigDecimal("0.00")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "change to valid correct amount for selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        for mrn <- journey.getMovementReferenceNumbers.get do {
          val totalAmount: BigDecimal              = journey.getReimbursementAmountForDeclaration(mrn)
          val taxCodes: Seq[(TaxCode, BigDecimal)] = journey.getReimbursementClaimsFor(mrn).toSeq
          for (taxCode, reimbursementAmount) <- taxCodes do {
            val paidAmount         = journey.getAmountPaidFor(mrn, taxCode).get
            val newCorrectedAmount = (paidAmount - reimbursementAmount) / 2
            val journeyEither      = journey.submitCorrectAmount(mrn, taxCode, newCorrectedAmount)
            val modifiedJourney    = journeyEither.getOrFail
            modifiedJourney.getReimbursementAmountForDeclaration(mrn) shouldBe (totalAmount + newCorrectedAmount)
          }
        }
      }
    }

    "change to invalid correct amount for selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        val mrn                                  = journey.answers.movementReferenceNumbers.get.head
        val taxCodes: Seq[(TaxCode, BigDecimal)] = journey.getReimbursementClaimsFor(mrn).toSeq
        for (taxCode, amount) <- taxCodes do {
          val paidAmount     = journey.getAmountPaidFor(mrn, taxCode).get
          val journeyEither1 = journey.submitCorrectAmount(mrn, taxCode, paidAmount)
          val journeyEither2 = journey.submitCorrectAmount(mrn, taxCode, paidAmount + BigDecimal("0.01"))
          val journeyEither3 = journey.submitCorrectAmount(mrn, taxCode, BigDecimal("-0.01"))
          journeyEither1 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          journeyEither2 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          journeyEither3 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
        }
      }
    }

    "change to valid correct amount for the tax code not in ACC14" in {
      forAll(completeJourneyGen) { journey =>
        val mrn           = journey.answers.movementReferenceNumbers.get.head
        val taxCodeSet    = journey.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val wrongTaxCode  = TaxCodes.all.find(taxCode => !taxCodeSet.contains(taxCode)).getOrElse(TaxCode.NI633)
        val journeyEither = journey.submitCorrectAmount(mrn, wrongTaxCode, BigDecimal("10.00"))
        journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
      }
    }

    "submit valid claim amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A00, BigDecimal("6.66")))

      journeyEither.isRight                               shouldBe true
      journeyEither.getOrFail.getTotalReimbursementAmount shouldBe BigDecimal("6.66")
    }

    "submit valid claim amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A80, BigDecimal("6.66")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "return left when submitting valid claim amount with missing display declaration" in {
      val journeyEither = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitClaimAmount(exampleMrn, TaxCode.A80, BigDecimal("5.00"))

      journeyEither shouldBe Left("submitCorrectAmount.missingDisplayDeclaration")
    }

    "return left when submitting valid claim amount for with no tax code selected" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A00, BigDecimal("5.00")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotSelectedYet")
    }

    "submit invalid claim amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration        = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))

      val journeyEitherTestZero     =
        declaration.flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A00, BigDecimal("0.00")))
      val journeyEitherTestNegative =
        declaration.flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A00, BigDecimal("-0.01")))
      val journeyEitherTestGreater  =
        declaration.flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A00, BigDecimal("10.01")))

      journeyEitherTestZero     shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
      journeyEitherTestNegative shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
      journeyEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
    }

    "submit invalid claim amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitClaimAmount(exampleMrn, TaxCode.A80, BigDecimal("0.00")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "change to valid claim amount for selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        for mrn <- journey.getMovementReferenceNumbers.get do {
          val totalAmount: BigDecimal              = journey.getReimbursementAmountForDeclaration(mrn)
          val taxCodes: Seq[(TaxCode, BigDecimal)] = journey.getReimbursementClaimsFor(mrn).toSeq
          for (taxCode, reimbursementAmount) <- taxCodes do {
            val newClaimAmount  = reimbursementAmount / 2
            val journeyEither   = journey.submitClaimAmount(mrn, taxCode, newClaimAmount)
            val modifiedJourney = journeyEither.getOrFail
            modifiedJourney.getReimbursementAmountForDeclaration(mrn) shouldBe (totalAmount - newClaimAmount)
          }
        }
      }
    }

    "change to invalid claim amount for selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        val mrn                                  = journey.answers.movementReferenceNumbers.get.head
        val taxCodes: Seq[(TaxCode, BigDecimal)] = journey.getReimbursementClaimsFor(mrn).toSeq
        for (taxCode, amount) <- taxCodes do {
          val paidAmount     = journey.getAmountPaidFor(mrn, taxCode).get
          val journeyEither1 = journey.submitClaimAmount(mrn, taxCode, BigDecimal("0.00"))
          val journeyEither2 = journey.submitClaimAmount(mrn, taxCode, paidAmount + BigDecimal("0.01"))
          val journeyEither3 = journey.submitClaimAmount(mrn, taxCode, BigDecimal("-0.01"))
          journeyEither1 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          journeyEither2 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          journeyEither3 shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
        }
      }
    }

    "change to valid claim amount for the tax code not in ACC14" in {
      forAll(completeJourneyGen) { journey =>
        val mrn           = journey.answers.movementReferenceNumbers.get.head
        val taxCodeSet    = journey.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val wrongTaxCode  = TaxCodes.all.find(taxCode => !taxCodeSet.contains(taxCode)).getOrElse(TaxCode.NI633)
        val journeyEither = journey.submitCorrectAmount(mrn, wrongTaxCode, BigDecimal("10.00"))
        journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
      }
    }

    "change bankAccountDetails in a complete journey" in {
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
              val initialJourney = RejectedGoodsMultipleJourney
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, updatedDd)
                .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, taxCodes))
                .getOrFail
              val journeyToTest  = ndrcDetails.dropRight(1).foldLeft(initialJourney) { case (journey, ndrcDetails) =>
                journey.submitCorrectAmount(exampleMrn, TaxCode(ndrcDetails.taxType), 1).getOrFail
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
              val journey   = RejectedGoodsMultipleJourney
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, updatedDd)
                .getOrFail

              journey.hasCompleteReimbursementClaims shouldBe false
            }
        }
      }
    }

    "validate if any subsidy payment method is in the declaration" in {

      import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport.withSomeSubsidiesPaymentMethod

      val declaration =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
          .withSomeSubsidiesPaymentMethod()

      val journey = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
        .getOrFail

      OverpaymentsMultipleJourney.Checks.declarationsHasNoSubsidyPayments.apply(
        journey
      ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)

    }

    "remove MRN and display declaration" in {
      val mrnDisplayDec1 = mrnWithDisplayDeclarationGen.sample.get
      val mrnDisplayDec2 = mrnWithDisplayDeclarationGen.sample.get
      val mrnDisplayDec3 = mrnWithDisplayDeclarationGen.sample.get
      val journey        =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(mrnDisplayDec1._1, mrnDisplayDec1._2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, mrnDisplayDec2._1, mrnDisplayDec2._2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(2, mrnDisplayDec3._1, mrnDisplayDec3._2)
          .getOrFail

      journey.getDisplayDeclarationFor(mrnDisplayDec3._1) shouldBe Some(mrnDisplayDec3._2)

      val modifiedJourney = journey.removeMovementReferenceNumberAndDisplayDeclaration(mrnDisplayDec3._1)

      modifiedJourney.getOrFail.getDisplayDeclarationFor(mrnDisplayDec3._1) shouldBe None
    }

    "return left when attempting to remove first MRN and display declaration" in {
      val mrnDisplayDec1 = mrnWithDisplayDeclarationGen.sample.get
      val mrnDisplayDec2 = mrnWithDisplayDeclarationGen.sample.get
      val journey        =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(mrnDisplayDec1._1, mrnDisplayDec1._2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, mrnDisplayDec2._1, mrnDisplayDec2._2)
          .getOrFail

      journey.removeMovementReferenceNumberAndDisplayDeclaration(mrnDisplayDec1._1) shouldBe Left(
        "removeMovementReferenceNumberAndDisplayDeclaration.cannotRemoveFirstMRN"
      )
    }

    "return left when attempting to remove second MRN and display declaration" in {
      val mrnDisplayDec1 = mrnWithDisplayDeclarationGen.sample.get
      val mrnDisplayDec2 = mrnWithDisplayDeclarationGen.sample.get
      val journey        =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(mrnDisplayDec1._1, mrnDisplayDec1._2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, mrnDisplayDec2._1, mrnDisplayDec2._2)
          .getOrFail

      journey.removeMovementReferenceNumberAndDisplayDeclaration(mrnDisplayDec2._1) shouldBe Left(
        "removeMovementReferenceNumberAndDisplayDeclaration.cannotRemoveSecondMRN"
      )
    }

    "return left when attempting to remove MRN and display declaration that doesn't exist" in {
      val mrnDisplayDec1 = mrnWithDisplayDeclarationGen.sample.get
      val mrnDisplayDec2 = mrnWithDisplayDeclarationGen.sample.get
      val journey        =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(mrnDisplayDec1._1, mrnDisplayDec1._2)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, mrnDisplayDec2._1, mrnDisplayDec2._2)
          .getOrFail

      journey.removeMovementReferenceNumberAndDisplayDeclaration(exampleMrn) shouldBe Left(
        "removeMovementReferenceNumberAndDisplayDeclaration.notFound"
      )
    }

    "remove unsupported tax codes" in {
      val journey = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(
          exampleMrn,
          exampleDisplayDeclarationWithSomeUnsupportedCode
        )
        .getOrFail
        .removeUnsupportedTaxCodes()

      journey.containsUnsupportedTaxCode shouldBe false
    }

    "remove bank account details" in {
      val journeyEither = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitBankAccountDetails(BankAccountGen.genBankAccountDetails.sample.get)
        .getOrFail
        .removeBankAccountDetails()

      journeyEither.answers.bankAccountDetails shouldBe None
    }

    "remove bank account details when submitting a different payeeType" in {
      val journey = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitPayeeType(PayeeType.Consignee)
        .getOrFail
        .submitBankAccountDetails(exampleBankAccountDetails)
        .getOrFail
        .submitPayeeType(PayeeType.Declarant)

      journey.getOrFail.answers.bankAccountDetails shouldBe None
    }

    "keep bank account details when submitting the same payeeType" in {
      val journey = OverpaymentsMultipleJourney
        .empty(exampleEori)
        .submitPayeeType(PayeeType.Consignee)
        .getOrFail
        .submitBankAccountDetails(exampleBankAccountDetails)
        .getOrFail
        .submitPayeeType(PayeeType.Consignee)

      journey.getOrFail.answers.bankAccountDetails shouldBe Some(exampleBankAccountDetails)
    }

    "get display declaration by index" in {
      val displayDeclaration  = buildDisplayDeclaration(id = exampleMrnAsString)
      val displayDeclaration2 = buildDisplayDeclaration(id = anotherExampleMrn.value)
      val journey             =
        OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(1, anotherExampleMrn, displayDeclaration2)
          .getOrFail

      journey.getNthDisplayDeclaration(1) shouldBe Some(displayDeclaration2)
    }

    "needsDuplicateMrnAndDeclaration" when {
      "basis of claim is not DuplicateEntry return false" in {
        val journey = OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectEoriAndDan)

        journey.needsDuplicateMrnAndDeclaration shouldBe false
      }

      "basis of claim is DuplicateEntry return true" in {
        val journey = OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry)

        journey.needsDuplicateMrnAndDeclaration shouldBe true
      }
    }

    "isAllSelectedDutiesAreCMAEligible" when {
      "all entries are CMA eligible return true" in {
        val displayDec = displayDeclarationCMAEligibleGen.sample.get

        val journey = OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDec.getMRN, displayDec)
          .getOrFail

        journey.isAllSelectedDutiesAreCMAEligible                    shouldBe true
        journey.isAllSelectedDutiesAreCMAEligible(displayDec.getMRN) shouldBe true
      }

      "not all entries are CMA eligible return false" in {
        val displayDecCMA    = displayDeclarationCMAEligibleGen.sample.get
        val displayDecNotCMA = displayDeclarationNotCMAEligibleGen.sample.get

        val journey = OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDecCMA.getMRN, displayDecCMA)
          .getOrFail
          .submitMovementReferenceNumberAndDeclaration(displayDecNotCMA.getMRN, displayDecNotCMA)
          .getOrFail

        journey.isAllSelectedDutiesAreCMAEligible                          shouldBe false
        journey.isAllSelectedDutiesAreCMAEligible(displayDecNotCMA.getMRN) shouldBe false
      }
    }

    "getAvailableDuties" when {
      "there are available duties return tax code with cmaEligible" in {
        val details         = Seq((TaxCode.A00, BigDecimal(200), true))
        val expectedDetails = Seq((TaxCode.A00, true))
        val displayDec      = buildDisplayDeclaration(dutyDetails = details)

        val journey = OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDec.getMRN, displayDec)
          .getOrFail

        journey.getAvailableDuties(displayDec.getMRN) shouldBe expectedDetails
      }

      "there are no duties return empty sequence" in {
        val displayDec = buildDisplayDeclaration(dutyDetails = Seq.empty)

        val journey = OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDec.getMRN, displayDec)
          .getOrFail

        journey.getAvailableDuties(displayDec.getMRN) shouldBe Seq.empty
      }

      "there are no matching duties return empty sequence" in {
        val details    = Seq((TaxCode("foo"), BigDecimal(200), true))
        val displayDec = buildDisplayDeclaration(dutyDetails = details)

        val journey = OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDec.getMRN, displayDec)
          .getOrFail

        journey.getAvailableDuties(displayDec.getMRN) shouldBe Seq.empty
      }
    }
  }
}
