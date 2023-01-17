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

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.RetrievedUserTypeGen.authenticatedUserGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._

import OverpaymentsSingleJourneyGenerators._
import JourneyValidationErrors._
import org.scalacheck.ShrinkLowPriority

class OverpaymentsSingleJourneySpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "OverpaymentsSingleJourney" should {
    "have an empty instance" in {
      emptyJourney.answers.userEoriNumber                   shouldBe exampleEori
      emptyJourney.answers.bankAccountDetails               shouldBe None
      emptyJourney.answers.bankAccountType                  shouldBe None
      emptyJourney.answers.basisOfClaim                     shouldBe None
      emptyJourney.answers.duplicateMovementReferenceNumber shouldBe None
      emptyJourney.answers.duplicateDisplayDeclaration      shouldBe None
      emptyJourney.answers.contactAddress                   shouldBe None
      emptyJourney.answers.contactDetails                   shouldBe None
      emptyJourney.answers.contactAddress                   shouldBe None
      emptyJourney.answers.declarantEoriNumber              shouldBe None
      emptyJourney.answers.additionalDetails                shouldBe None
      emptyJourney.answers.displayDeclaration               shouldBe None
      emptyJourney.answers.consigneeEoriNumber              shouldBe None
      emptyJourney.answers.correctedAmounts                 shouldBe None
      emptyJourney.answers.reimbursementMethod              shouldBe None
      emptyJourney.answers.selectedDocumentType             shouldBe None
      emptyJourney.answers.supportingEvidences              shouldBe Seq.empty
      emptyJourney.answers.checkYourAnswersChangeMode       shouldBe false
      emptyJourney.answers.dutiesChangeMode                 shouldBe false
      emptyJourney.getNdrcDetails                           shouldBe None
      emptyJourney.getSelectedDuties                        shouldBe None
      emptyJourney.isAllSelectedDutiesAreCMAEligible        shouldBe false
      emptyJourney.hasCompleteReimbursementClaims           shouldBe false
      emptyJourney.hasCompleteSupportingEvidences           shouldBe true
      emptyJourney.hasCompleteAnswers                       shouldBe false
      emptyJourney.toOutput.isLeft                          shouldBe true
      emptyJourney.isFinalized                              shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeJourneyGen) { journey =>
        OverpaymentsSingleJourney.validator.apply(journey) shouldBe Right(())
        journey.answers.checkYourAnswersChangeMode         shouldBe true
        journey.hasCompleteReimbursementClaims             shouldBe true
        journey.hasCompleteSupportingEvidences             shouldBe true
        journey.hasCompleteAnswers                         shouldBe true
        journey.isFinalized                                shouldBe false

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
        output.reimbursementClaims      shouldBe journey.getReimbursementClaims
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

    "accept submission of a new MRN" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, decl) =>
        val journey = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(mrn, decl)
          .getOrFail
        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteReimbursementClaims                shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
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
        modifiedJourney.hasCompleteSupportingEvidences shouldBe true
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
        journey.hasCompleteSupportingEvidences                                                   shouldBe true
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
        modifiedJourney.hasCompleteSupportingEvidences  shouldBe true
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

      journeyEither shouldBe Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
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

      journeyEither shouldBe Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
    }

    "get contact details" should {
      "return the specified details if they have been entered" in {
        forAll(completeJourneyGen, authenticatedUserGen) { (journey, signedInUser) =>
          whenever(journey.answers.contactDetails.isDefined) {
            val result = journey.computeContactDetails(signedInUser)
            result shouldBe journey.answers.contactDetails
          }
        }
      }

      "return the consignee details if no specific details entered and the signed in user is the consignee and consignee details are present" in {
        forAll(
          buildCompleteJourneyGen(
            acc14ConsigneeMatchesUserEori = true,
            acc14DeclarantMatchesUserEori = false,
            submitContactDetails = false
          ),
          authenticatedUserGen
        ) { (journey, signedInUser) =>
          whenever(
            journey.answers.displayDeclaration.flatMap(_.getConsigneeDetails.flatMap(_.contactDetails)).isDefined
          ) {
            val expectedContact   = journey.answers.displayDeclaration
              .flatMap(_.getConsigneeDetails.flatMap(_.contactDetails))
              .getOrElse(fail("Failed to get contact details"))
            val calculatedContact = journey.computeContactDetails(signedInUser).get
            calculatedContact.fullName                 shouldBe expectedContact.contactName.getOrElse("")
            calculatedContact.emailAddress.value       shouldBe expectedContact.emailAddress.getOrElse(
              signedInUser.email.get.value
            )
            calculatedContact.phoneNumber.map(_.value) shouldBe expectedContact.telephone
          }
        }
      }

      "return the signed in user details if no specific details entered and the signed in user is the consignee and consignee details are not present" in {
        forAll(
          buildCompleteJourneyGen(
            acc14ConsigneeMatchesUserEori = true,
            acc14DeclarantMatchesUserEori = false,
            submitConsigneeDetails = false,
            submitContactDetails = false
          ),
          authenticatedUserGen
        ) { (journey, signedInUser) =>
          whenever(
            journey.answers.displayDeclaration.flatMap(_.getDeclarantDetails.contactDetails).isDefined &&
              journey.answers.displayDeclaration.flatMap(_.getConsigneeDetails.flatMap(_.contactDetails)).isEmpty
          ) {
            val calculatedContact = journey.computeContactDetails(signedInUser).get
            calculatedContact.fullName           shouldBe signedInUser.name
              .map(_.toFullName)
              .getOrElse(fail("No signed in user name present"))
            calculatedContact.emailAddress.value shouldBe signedInUser.email
              .map(_.value)
              .getOrElse(fail("No signed in user email present"))
            calculatedContact.phoneNumber        shouldBe None
          }
        }
      }

      "return the declarant details if no specific details entered and the signed in user is the declarant" in {
        forAll(
          buildCompleteJourneyGen(
            acc14ConsigneeMatchesUserEori = false,
            acc14DeclarantMatchesUserEori = true,
            submitConsigneeDetails = true,
            submitContactDetails = false
          ),
          authenticatedUserGen
        ) { (journey, signedInUser) =>
          whenever(
            journey.answers.displayDeclaration.flatMap(_.getDeclarantDetails.contactDetails).isDefined
          ) {
            val expectedContact   = journey.answers.displayDeclaration
              .flatMap(_.getDeclarantDetails.contactDetails)
              .getOrElse(fail("Failed to get contact details"))
            val calculatedContact = journey.computeContactDetails(signedInUser).get
            calculatedContact.fullName                 shouldBe expectedContact.contactName.getOrElse("")
            calculatedContact.emailAddress.value       shouldBe expectedContact.emailAddress.getOrElse(
              signedInUser.email.get.value
            )
            calculatedContact.phoneNumber.map(_.value) shouldBe expectedContact.telephone
          }
        }
      }

      "return the declarant details if no specific details entered and the signed in user is neither the consignee or declarant" in {
        forAll(
          buildCompleteJourneyGen(
            acc14ConsigneeMatchesUserEori = false,
            acc14DeclarantMatchesUserEori = false,
            submitConsigneeDetails = true,
            submitContactDetails = false
          ),
          authenticatedUserGen
        ) { (journey, signedInUser) =>
          whenever(
            journey.answers.displayDeclaration.flatMap(_.getDeclarantDetails.contactDetails).isDefined
          ) {
            val expectedContact   = journey.answers.displayDeclaration
              .flatMap(_.getDeclarantDetails.contactDetails)
              .getOrElse(fail("Failed to get contact details"))
            val calculatedContact = journey.computeContactDetails(signedInUser).get
            calculatedContact.fullName                 shouldBe expectedContact.contactName.getOrElse("")
            calculatedContact.emailAddress.value       shouldBe expectedContact.emailAddress.getOrElse(
              signedInUser.email.get.value
            )
            calculatedContact.phoneNumber.map(_.value) shouldBe expectedContact.telephone
          }
        }
      }
    }

    "get contact address" should {
      "return the specified details if they have been entered" in {
        forAll(completeJourneyGen) { journey =>
          whenever(journey.answers.contactAddress.isDefined) {
            journey.computeAddressDetails shouldBe journey.answers.contactAddress
          }
        }
      }

      "return the consignee address if no specific address entered and the signed in user is the consignee and consignee address is present" in {
        forAll(
          buildCompleteJourneyGen(
            submitContactAddress = false,
            acc14ConsigneeMatchesUserEori = true,
            acc14DeclarantMatchesUserEori = false
          )
        ) { journey =>
          val expectedAddress = journey.answers.displayDeclaration.flatMap(
            _.getConsigneeDetails.map(_.establishmentAddress.toContactAddress)
          )
          journey.computeAddressDetails shouldBe expectedAddress
        }
      }

      "return the declarant address if no specific address entered and the signed in user is the consignee and consignee address is not present" in {
        forAll(
          buildCompleteJourneyGen(
            submitContactAddress = false,
            acc14ConsigneeMatchesUserEori = true,
            submitConsigneeDetails = false
          )
        ) { journey =>
          val expectedAddress =
            journey.answers.displayDeclaration.map(_.getDeclarantDetails.establishmentAddress.toContactAddress)
          journey.computeAddressDetails shouldBe expectedAddress
        }
      }

      "return the declarant address if no specific address entered and the signed in user is the declarant" in {
        forAll(
          buildCompleteJourneyGen(
            submitContactAddress = false,
            acc14ConsigneeMatchesUserEori = false,
            acc14DeclarantMatchesUserEori = true
          )
        ) { journey =>
          val expectedAddress =
            journey.answers.displayDeclaration.map(_.getDeclarantDetails.establishmentAddress.toContactAddress)
          journey.computeAddressDetails shouldBe expectedAddress
        }
      }

      "return the declarant address if no specific address entered and the signed in user is neither the declarant or the consignee and declarant address is present" in {
        forAll(
          buildCompleteJourneyGen(
            submitContactAddress = false,
            acc14ConsigneeMatchesUserEori = false,
            acc14DeclarantMatchesUserEori = false
          )
        ) { journey =>
          val expectedAddress =
            journey.answers.displayDeclaration.map(_.getDeclarantDetails.establishmentAddress.toContactAddress)
          journey.computeAddressDetails shouldBe expectedAddress
        }
      }
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
        modifiedJourney.answers.contactDetails shouldBe Some(contactDetails)
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
        modifiedJourney.answers.contactAddress shouldBe Some(contactAddress)
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
          BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.DuplicateEntry - BasisOfOverpaymentClaim.IncorrectExciseValue
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

      journeyEither.isRight shouldBe true
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
        val invalidTaxCodeSet     = TaxCodes.all.take(6).toSeq
        val modifiedJourneyEither = journey.selectAndReplaceTaxCodeSetForReimbursement(invalidTaxCodeSet)
        modifiedJourneyEither shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
      }
    }

    "submit valid amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("5.00")))

      journeyEither.isRight shouldBe true
    }

    "submit valid amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails =
        Seq((TaxCode.A00, BigDecimal("10.00"), false), (TaxCode.A90, BigDecimal("20.00"), false))
      )
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A80, BigDecimal("5.00")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "submit invalid amount for selected tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val declaration        = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))

      val journeyEitherTestZero     = declaration.flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("10.00")))
      val journeyEitherTestNegative =
        declaration.flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("-10.00")))
      val journeyEitherTestGreater  =
        declaration.flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("20.00")))

      journeyEitherTestZero     shouldBe Left("submitCorrectAmount.invalidAmount")
      journeyEitherTestNegative shouldBe Left("submitCorrectAmount.invalidAmount")
      journeyEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidAmount")
    }

    "submit invalid amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = OverpaymentsSingleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(TaxCode.A80, BigDecimal("0.00")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "change to valid amount for selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        val totalAmount: BigDecimal              = journey.getTotalReimbursementAmount
        val taxCodes: Seq[(TaxCode, BigDecimal)] = journey.getSelectedTaxCodesWithCorrectAmount
        for ((taxCode, correctAmount) <- taxCodes) {
          val newCorrectAmount = correctAmount / 2
          val journeyEither    = journey.submitCorrectAmount(taxCode, newCorrectAmount)
          journeyEither.isRight shouldBe true
          val modifiedJourney = journeyEither.getOrFail
          modifiedJourney.getTotalReimbursementAmount shouldBe (totalAmount + newCorrectAmount)
        }
      }
    }

    "change to invalid amount for selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        val taxCodes: Seq[TaxCode] = journey.getSelectedDuties.get

        for (taxCode <- taxCodes) {
          val ndrcDetails   = journey.getNdrcDetailsFor(taxCode).get
          val newAmount     = BigDecimal(ndrcDetails.amount)
          val journeyEither = journey.submitCorrectAmount(taxCode, newAmount)

          journeyEither shouldBe Left("submitCorrectAmount.invalidAmount")
        }
      }
    }

    "change to valid amount for the tax code not in ACC14" in {
      forAll(completeJourneyGen) { journey =>
        val taxCodeSet    = journey.getNdrcDetails.map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val wrongTaxCode  = TaxCodes.all.find(taxCode => !taxCodeSet.contains(taxCode)).getOrElse(TaxCode.NI633)
        val journeyEither = journey.submitCorrectAmount(wrongTaxCode, BigDecimal("10.00"))
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
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.99")))
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
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
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
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.01")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))

      journeyEither.isRight shouldBe true
    }

    "fail submitting BankAccountTransfer as reimbursement method when NOT all duties are CMA eligible" in {
      val displayDeclarationNotCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), false)))
      val journeyEither                    =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationNotCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))

      journeyEither shouldBe Left("submitReimbursementMethod.notCMAEligible")
    }

    "submit bankAccountDetails and bankAccountType if reimbursement method is BankAccountTransfer" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.BankAccountTransfer))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.Business))

      journeyEither.isRight shouldBe true
    }

    "fail submitting bankAccountDetails if not needed" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(TaxCode.A00, BigDecimal("0.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethod.CurrentMonthAdjustment))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.Business))

      journeyEither shouldBe Left("submitBankAccountDetails.unexpected")
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

    "getNextNdrcDetailsToClaim" when {
      "return the next Ndrc Details to claim" in {
        forAll(displayDeclarationGen, Acc14Gen.genListNdrcDetails()) {
          (displayDeclaration: DisplayDeclaration, ndrcDetails: List[NdrcDetails]) =>
            whenever(ndrcDetails.size > 1 && ndrcDetails.map(_.taxType).toSet.size == ndrcDetails.size) {
              val taxCodes             = ndrcDetails.map(details => TaxCode(details.taxType))
              val drd                  = displayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
              val updatedDd            = displayDeclaration.copy(displayResponseDetail = drd)
              val journey              = OverpaymentsSingleJourney
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(exampleMrn, updatedDd)
                .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(taxCodes))
                .getOrFail
              val claimedReimbursement = journey.answers.correctedAmounts.get
              val nextDetails          = journey.getNextNdrcDetailsToClaim.get
              claimedReimbursement.get(TaxCode(nextDetails.taxType)) shouldBe Some(None)
              // Some states that the tax code exists and the inner None tells us that no claim amount has been submitted for it
            }
        }
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
                journey.submitCorrectAmount(TaxCode(ndrcDetails.taxType), 1).getOrFail
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
  }
}
