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
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyValidationErrors._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.AuthenticatedUserGen.authenticatedUserGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

class OverpaymentsScheduledJourneySpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority
    with Logging {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "OverpaymentsScheduledJourney" should {
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
      emptyJourney.answers.displayDeclaration                                     shouldBe None
      emptyJourney.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyJourney.answers.correctedAmounts                                       shouldBe None
      emptyJourney.answers.selectedDocumentType                                   shouldBe None
      emptyJourney.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyJourney.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyJourney.answers.dutiesChangeMode                                       shouldBe false
      emptyJourney.getNdrcDetails                                                 shouldBe None
      emptyJourney.getSelectedDutyTypes                                           shouldBe None
      emptyJourney.hasCompleteReimbursementClaims                                 shouldBe false
      emptyJourney.hasCompleteSupportingEvidences                                 shouldBe true
      emptyJourney.hasCompleteAnswers                                             shouldBe false
      emptyJourney.toOutput.isLeft                                                shouldBe true
      emptyJourney.isFinalized                                                    shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeJourneyGen) { journey =>
        OverpaymentsScheduledJourney.validator.apply(journey) shouldBe Right(())
        journey.answers.checkYourAnswersChangeMode            shouldBe true
        journey.hasCompleteReimbursementClaims                shouldBe true
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteAnswers                            shouldBe true
        journey.isFinalized                                   shouldBe false
        journey.getNdrcDetails                                shouldBe defined

        val output = journey.toOutput.fold(e => fail(e.mkString("\n")), identity)

        output.movementReferenceNumber  shouldBe journey.answers.movementReferenceNumber.get
        output.claimantType             shouldBe journey.getClaimantType
        output.basisOfClaim             shouldBe journey.answers.basisOfClaim.get
        output.additionalDetails        shouldBe journey.answers.additionalDetails.get
        output.reimbursementMethod      shouldBe ReimbursementMethod.BankAccountTransfer
        output.reimbursementClaims      shouldBe journey.getReimbursementClaims
        output.supportingEvidences      shouldBe journey.answers.supportingEvidences.map(EvidenceDocument.from)
        output.bankAccountDetails       shouldBe journey.answers.bankAccountDetails
        output.claimantInformation.eori shouldBe journey.answers.userEoriNumber
      }
    }

    "fail produce an output if journey is incomplete" in {
      forAll(buildJourneyGen(submitContactDetails = false).map(_.getOrFail)) { journey =>
        journey.hasCompleteAnswers shouldBe false
        journey.isFinalized        shouldBe false
        val result = journey.toOutput
        result.isLeft shouldBe true
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

    "fail finalizing journey when incomplete" in {
      forAll(buildJourneyGen(submitContactDetails = false).map(_.getOrFail)) { journey =>
        journey.hasCompleteAnswers shouldBe false
        journey.isFinalized        shouldBe false
        val result = journey.finalizeJourneyWith("foo-123-abc")
        result.isLeft shouldBe true
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
        modifiedJourney.hasCompleteSupportingEvidences  shouldBe true
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
        OverpaymentsScheduledJourney
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
        OverpaymentsScheduledJourney
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
        OverpaymentsScheduledJourney
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
        OverpaymentsScheduledJourney
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
        OverpaymentsScheduledJourney
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
        OverpaymentsScheduledJourney
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
        OverpaymentsScheduledJourney
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
        OverpaymentsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        OverpaymentsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
    }

    "fail if submitted declarant EORI is not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val journeyEither      =
        OverpaymentsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        OverpaymentsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
    }

    "get contact details" should {
      "return the specified details if they have been entered" in {
        forAll(completeJourneyGen, authenticatedUserGen) { (journey, signedInUser) =>
          whenever(journey.answers.contactDetails.isDefined) {
            val result = journey.computeContactDetails(signedInUser, signedInUser.asVerifiedEmail)
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
            val calculatedContact = journey.computeContactDetails(signedInUser, signedInUser.asVerifiedEmail).get
            calculatedContact.fullName                 shouldBe expectedContact.contactName.getOrElse("")
            calculatedContact.emailAddress.value       shouldBe expectedContact.maybeEmailAddress.getOrElse(
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
            val calculatedContact = journey.computeContactDetails(signedInUser, signedInUser.asVerifiedEmail).get
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
            val calculatedContact = journey.computeContactDetails(signedInUser, signedInUser.asVerifiedEmail).get
            calculatedContact.fullName                 shouldBe expectedContact.contactName.getOrElse("")
            calculatedContact.emailAddress.value       shouldBe expectedContact.maybeEmailAddress.getOrElse(
              signedInUser.email.get.value
            )
            calculatedContact.phoneNumber.map(_.value) shouldBe expectedContact.telephone
          }
        }
      }

      "return the signed in user details if no specific details entered and the signed in user is neither the consignee or declarant" in {
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
            val calculatedContact = journey.computeContactDetails(signedInUser, signedInUser.asVerifiedEmail).get
            calculatedContact.fullName                 shouldBe signedInUser.name.map(_.toFullName).getOrElse("")
            calculatedContact.emailAddress.value       shouldBe signedInUser.email.map(_.value).getOrElse("")
            calculatedContact.phoneNumber.map(_.value) shouldBe None
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
          OverpaymentsScheduledJourney
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
        val journey = OverpaymentsScheduledJourney.empty(exampleEori).submitContactAddress(contactAddress)

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
        val journey = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .submitBasisOfClaim(basisOfClaim)
        journey.answers.basisOfClaim shouldBe Some(basisOfClaim)
      }
    }

    "change basis of claim" in {
      forAll(
        completeJourneyGen,
        Gen.oneOf(
          BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.IncorrectExciseValue
        )
      ) { (journey, basisOfClaim) =>
        val modifiedJourney = journey.submitBasisOfClaim(basisOfClaim)

        modifiedJourney.hasCompleteAnswers           shouldBe true
        modifiedJourney.toOutput.map(_.basisOfClaim) shouldBe Right(basisOfClaim)
      }
    }

    "submit additional details" in {
      val journey = OverpaymentsScheduledJourney
        .empty(exampleEori)
        .submitAdditionalDetails("foo bar")

      journey.answers.additionalDetails shouldBe Some("foo bar")
    }

    "change additional details" in {
      forAll(completeJourneyGen, Gen.asciiPrintableStr) { (journey, additionalDetails) =>
        val modifiedJourney = journey.submitAdditionalDetails(additionalDetails)

        modifiedJourney.hasCompleteAnswers                shouldBe true
        modifiedJourney.toOutput.map(_.additionalDetails) shouldBe Right(additionalDetails)
      }
    }

    "select duty types for reimbursement when none yet selected" in {
      forAll(dutyTypesGen) { (dutyTypes: Seq[DutyType]) =>
        val journey = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .getOrFail

        journey.getSelectedDutyTypes shouldBe Some(dutyTypes)
      }
    }

    "replace duty types for reimbursement" in {
      forAll(dutyTypesGen, dutyTypesGen) { (dutyTypes1: Seq[DutyType], dutyTypes2: Seq[DutyType]) =>
        val journey = OverpaymentsScheduledJourney
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
        val journey   = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            journey =>
              (tc: (DutyType, Seq[TaxCode])) => journey.selectAndReplaceTaxCodeSetForReimbursement(tc._1, tc._2)
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
          val journey = OverpaymentsScheduledJourney
            .empty(exampleEori)
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .flatMapEach(
              Seq(dutyType -> taxCodes1),
              journey =>
                (tc: (DutyType, Seq[TaxCode])) => journey.selectAndReplaceTaxCodeSetForReimbursement(tc._1, tc._2)
            )
            .flatMapEach(
              Seq(dutyType -> taxCodes2),
              journey =>
                (tc: (DutyType, Seq[TaxCode])) => journey.selectAndReplaceTaxCodeSetForReimbursement(tc._1, tc._2)
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
        val result    = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            journey =>
              (tc: (DutyType, Seq[TaxCode])) =>
                journey.selectAndReplaceTaxCodeSetForReimbursement(tc._1, Seq(TaxCode.A00, TaxCode.A50))
          )

        result shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesDoesNotMatchDutyType")
      }
    }

    "select invalid duty types for reimbursement" in {
      forAll(dutyTypesWithTaxCodesGen) { (dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]) =>
        val dutyTypes = dutyTypesWithTaxCodes.map(_._1)
        val result    = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(DutyTypes.all.toSet.diff(dutyTypes.toSet).toSeq)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            journey =>
              (tc: (DutyType, Seq[TaxCode])) => journey.selectAndReplaceTaxCodeSetForReimbursement(tc._1, tc._2)
          )

        result shouldBe Left("selectTaxCodeSetForReimbursement.dutyTypeNotSelectedBefore")
      }
    }

    "return left if empty duty types selection" in {
      val dutyTypes: Seq[DutyType] = Seq.empty
      val result                   = OverpaymentsScheduledJourney
        .empty(exampleEori)
        .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)

      result shouldBe Left("selectAndReplaceDutyTypeSetForReimbursement.emptySelection")
    }

    "return left if empty tax type selection" in {
      forAll(dutyTypesGen) { (dutyTypes: Seq[DutyType]) =>
        val taxTypes: Seq[TaxCode] = List.empty
        val result                 = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypes,
            journey => (dt: DutyType) => journey.selectAndReplaceTaxCodeSetForReimbursement(dt, taxTypes)
          )

        result shouldBe Left("selectTaxCodeSetForReimbursement.emptySelection")
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
            .selectAndReplaceTaxCodeSetForReimbursement(dutyType, taxCodes)
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

        val result                                               = journey
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForReimbursement(d._1, d._2)
          )
          .getOrFail

        result.hasCompleteReimbursementClaims shouldBe true
        result.hasCompleteAnswers             shouldBe true
      }
    }

    "submit valid amounts for selected duty types and tax codes" in {
      forAll(dutyTypesWithTaxCodesWithClaimAmountsGen) { data =>
        val dutyTypes: Seq[DutyType]                                              = data.map(_._1)
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                  = data.map { case (dt, tcs) => dt -> tcs.map(_._1) }
        val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)] = data.flatMap { case (dt, tca) =>
          tca.map { case (tc, pa, ca) => (dt, tc, pa, ca) }
        }
        val expectedTotalReimbursementAmount                                      =
          taxCodesWithAmounts.map { case (_, _, pa, ca) => pa - ca }.sum

        val journey                                                               = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForReimbursement(d._1, d._2)
          )
          .flatMapEach(
            taxCodesWithAmounts,
            j => (d: (DutyType, TaxCode, BigDecimal, BigDecimal)) => j.submitCorrectAmount(d._1, d._2, d._3, d._4)
          )
          .getOrFail

        journey.getSelectedDutyTypes                      shouldBe Some(dutyTypes)
        dutyTypesWithTaxCodes.foreach { case (dutyType, taxCodes) =>
          journey.getSelectedDutiesFor(dutyType).get shouldBe taxCodes
          taxCodes.foreach(taxCode => journey.isDutySelected(dutyType, taxCode))
          TaxCodes.allExcept(taxCodes.toSet).foreach(taxCode => !journey.isDutySelected(dutyType, taxCode))
        }
        journey.getReimbursementClaims.map(_._2.size).sum shouldBe taxCodesWithAmounts.size
        journey.getTotalReimbursementAmount               shouldBe expectedTotalReimbursementAmount
      }
    }

    "return false if at least one of the claimed tax code do not have a value specified" in {
      forAll(dutyTypesWithTaxCodesWithClaimAmountsGen) { data =>
        val dutyTypes: Seq[DutyType]                                              = data.map(_._1)
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                  = data.map { case (dt, tcs) => dt -> tcs.map(_._1) }
        val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)] = data.flatMap { case (dt, tca) =>
          tca.map { case (tc, pa, ca) => (dt, tc, pa, ca) }
        }

        val journey = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForReimbursement(d._1, d._2)
          )
          .flatMapEach(
            taxCodesWithAmounts.dropRight(1),
            j => (d: (DutyType, TaxCode, BigDecimal, BigDecimal)) => j.submitCorrectAmount(d._1, d._2, d._3, d._4)
          )
          .getOrFail

        journey.hasCompleteReimbursementClaims shouldBe false
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

        val result = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForReimbursement(d._1, d._2)
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

        val result = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForReimbursement(d._1, d._2)
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

      val availableClaimTypesNotNi      =
        BasisOfOverpaymentClaimsList
          .withoutDuplicateEntry()
          .excludeNorthernIrelandClaims(false, Some(displayDeclaration))
      val availableClaimTypesIncludesNi =
        BasisOfOverpaymentClaimsList
          .withoutDuplicateEntry()
          .excludeNorthernIrelandClaims(true, Some(displayDeclaration))

      val journey = OverpaymentsScheduledJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
        .getOrFail

      journey.getDocumentTypesIfRequired shouldBe Some(availableDocumentTypes)

      journey.getAvailableClaimTypes shouldBe availableClaimTypesNotNi

      val journeyNotNi = journey
        .submitWhetherNorthernIreland(false)

      val journeyNi = journey
        .submitWhetherNorthernIreland(true)

      journeyNotNi.getAvailableClaimTypes shouldBe availableClaimTypesNotNi

      journeyNi.getAvailableClaimTypes shouldBe availableClaimTypesIncludesNi

      for (document <- availableDocumentTypes) {
        val result = journey.submitDocumentTypeSelection(document)

        result.getSelectedDocumentType shouldBe Some(document)
      }
    }

    "reject submit invalid amount for valid selected tax code" in {
      forAll(dutyTypesWithTaxCodesWithClaimAmountsGen) { data =>
        val dutyTypes: Seq[DutyType]                                              = data.map(_._1)
        val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                  = data.map { case (dt, tcs) => dt -> tcs.map(_._1) }
        val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)] = data.flatMap { case (dt, tca) =>
          tca.map { case (tc, pa, ca) => (dt, tc, pa, ca) }
        }

        val result = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
          .flatMapEach(
            dutyTypesWithTaxCodes,
            j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForReimbursement(d._1, d._2)
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

    "submit bankAccountDetails and bankAccountType" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        OverpaymentsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceDutyTypeSetForReimbursement(DutyTypes.custom))
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(DutyType.UkDuty, Seq(TaxCode.A00)))
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
      val journey = OverpaymentsScheduledJourney.empty(exampleEori)

      val dutyTypes       = Seq(DutyType.EuDuty, DutyType.Beer, DutyType.Biofuels)
      val modifiedJourney =
        journey.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes).getOrFail

      modifiedJourney.findNextSelectedDutyAfter(DutyType.Beer)     shouldBe Some(DutyType.Biofuels)
      modifiedJourney.findNextSelectedDutyAfter(DutyType.EuDuty)   shouldBe Some(DutyType.Beer)
      modifiedJourney.findNextSelectedDutyAfter(DutyType.Biofuels) shouldBe None
    }

    "find next duty type" in {
      val journey = OverpaymentsScheduledJourney.empty(exampleEori)
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
      val journey = OverpaymentsScheduledJourney.empty(exampleEori)
      forAll(dutyTypesWithTaxCodesGen) { dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])] =>
        whenever(dutyTypesWithTaxCodes.nonEmpty && dutyTypesWithTaxCodes.forall(_._2.nonEmpty)) {

          val dutyTypes: Seq[DutyType] = dutyTypesWithTaxCodes.map(_._1)
          val modifiedJourney          =
            journey
              .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
              .flatMapEach(
                dutyTypesWithTaxCodes,
                j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForReimbursement(d._1, d._2)
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

      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      var journey =
        OverpaymentsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
          .flatMap(_.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes))
          .getOrFail

      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      var previousDuty: Option[DutyType] = None

      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      var previousTaxCode: Option[TaxCode] = None

      for ((dutyType, taxCodes) <- taxCodesWithTypes.toSeq.sortBy(_._1)) {
        journey.findNextDutyToSelectDuties shouldBe Some(dutyType)

        previousDuty.foreach { pdt =>
          journey.findNextSelectedDutyAfter(pdt) shouldBe Some(dutyType)
        }

        journey = journey
          .selectAndReplaceTaxCodeSetForReimbursement(dutyType, taxCodes)
          .getOrFail

        previousTaxCode = None

        for (taxCode <- taxCodes.sorted) {
          val ndrcDetails = nextDetailsList.find(d => d.taxType === taxCode.value).get
          val amount      = BigDecimal(ndrcDetails.amount)

          journey.getNdrcDetailsFor(taxCode) shouldBe Some(ndrcDetails)

          journey.getNextNdrcDetailsToClaim shouldBe Some(ndrcDetails)

          journey.getReimbursementFor(dutyType, taxCode) shouldBe None

          journey = journey
            .submitCorrectAmount(dutyType, taxCode, amount, ZERO)
            .getOrFail

          journey.getReimbursementFor(dutyType, taxCode) shouldBe Some(AmountPaidWithCorrect(amount, ZERO))

          previousTaxCode.foreach { ptc =>
            journey.findNextSelectedTaxCodeAfter(dutyType, ptc) shouldBe Some((dutyType, taxCode))
          }

          previousTaxCode = Some(taxCode)
        }

        previousDuty = Some(dutyType)
      }

      journey.findNextDutyToSelectDuties shouldBe None
      journey.getNextNdrcDetailsToClaim  shouldBe None

      previousDuty.foreach { pdt =>
        journey.findNextSelectedDutyAfter(pdt) shouldBe None

        previousTaxCode.foreach { ptc =>
          journey.findNextSelectedTaxCodeAfter(pdt, ptc) shouldBe None
        }
      }
    }

    "get and update selected duties" in {
      forAll(dutyTypesWithTaxCodesGen) { dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])] =>
        whenever(dutyTypesWithTaxCodes.nonEmpty && dutyTypesWithTaxCodes.forall(_._2.nonEmpty)) {

          val dutyTypes: Seq[DutyType] = dutyTypesWithTaxCodes.map(_._1)

          val journey = OverpaymentsScheduledJourney
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .getOrFail

          journey.getSelectedDuties shouldBe Map.empty

          val modifiedJourney = journey
            .selectAndReplaceDutyTypeSetForReimbursement(dutyTypes)
            .flatMapEach(
              dutyTypesWithTaxCodes,
              j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForReimbursement(d._1, d._2)
            )
            .getOrFail

          val selectedDuties = dutyTypesWithTaxCodes.toMap

          modifiedJourney.getSelectedDuties shouldBe selectedDuties

          val updateDuties = modifiedJourney
            .withDutiesChangeMode(true)

          updateDuties.answers.dutiesChangeMode shouldBe true

        }
      }
    }

    "receiveUploadedFiles fails when nonce is not matching the journey nonce" in {
      val journey = OverpaymentsScheduledJourney.empty(exampleEori)
      val result  = journey.receiveUploadedFiles(UploadDocumentType.ProofOfAuthority, Nonce.random, Seq.empty)
      result shouldBe Left("receiveUploadedFiles.invalidNonce")
    }

    "receiveUploadedFiles fills in missing documentType" in {
      val journey      = OverpaymentsScheduledJourney.empty(exampleEori)
      val uploadedFile = buildUploadDocument("foo").copy(cargo = None)
      val result       =
        journey
          .receiveUploadedFiles(UploadDocumentType.ProofOfAuthority, journey.answers.nonce, Seq(uploadedFile))
          .getOrFail
      result.answers.supportingEvidences.head shouldBe uploadedFile.copy(cargo =
        Some(UploadDocumentType.ProofOfAuthority)
      )
    }
    "tryBuildFrom should return upload type other" in {
      val specialJourneyGen: Gen[OverpaymentsScheduledJourney] = buildJourneyGenWithoutSupportingEvidence()
      forAll(specialJourneyGen) { journey: OverpaymentsScheduledJourney =>
        journey.answers.supportingEvidences.foreach(uploadedFile =>
          uploadedFile.cargo.get shouldBe UploadDocumentType.Other
        )
      }
    }
    "receiveScheduledDocument fails when nonce is not matching the journey nonce" in {
      val journey = OverpaymentsScheduledJourney.empty(exampleEori)
      val result  = journey.receiveScheduledDocument(Nonce.random, buildUploadDocument("foo"))
      result shouldBe Left("receiveScheduledDocument.invalidNonce")
    }
    "removeScheduledDocument" in {
      val journey         = OverpaymentsScheduledJourney.empty(exampleEori)
      val nonce           = journey.answers.nonce
      val uploadedFile    = buildUploadDocument("foo").copy(cargo = None)
      val modifiedJourney = journey.receiveScheduledDocument(nonce, uploadedFile)

      modifiedJourney.getOrFail.answers.scheduledDocument shouldBe Some(uploadedFile)
      val result =
        journey.removeScheduledDocument
      result.answers.scheduledDocument shouldBe None
    }

    "check if journey instance is equal to itself" in {
      val journey    = OverpaymentsScheduledJourney
        .empty(exampleEori)
      val trueResult = journey
      journey.equals(trueResult) shouldBe true

      val falseResult = OverpaymentsSingleJourney
        .empty(exampleEori)
      journey.equals(falseResult) shouldBe false
    }

    "return journey hash code" in {
      val journey = OverpaymentsScheduledJourney
        .empty(exampleEori)

      journey.answers.hashCode() shouldBe journey.hashCode()
    }

    "serialises and deserialises json" in {
      forAll(completeJourneyGen) { data =>
        val journey: OverpaymentsScheduledJourney = data

        val json: JsValue = Json.toJson(journey)
        val result        = Json.parse(json.toString()).asOpt[OverpaymentsScheduledJourney]

        result  shouldBe defined
        journey shouldBe result.get
      }
    }

    "check for invalid journey" in {
      forAll(completeJourneyGen) { data =>
        val invalidJourney: OverpaymentsScheduledJourney = data.removeScheduledDocument

        invalidJourney.toOutput shouldBe Left(
          List("missingScheduledDocument")
        )
      }
    }

    "validate subsidy payment methods in declaration" when {

      import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport

      "feature not enabled" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSubsidiesPaymentMethod()

        val journey = OverpaymentsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe None

        OverpaymentsScheduledJourney.Checks.shouldBlockSubsidiesAndDeclarationHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Valid
      }

      "feature enabled" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSubsidiesPaymentMethod()

        val journey = OverpaymentsScheduledJourney
          .empty(exampleEori, features = Some(OverpaymentsScheduledJourney.Features(shouldBlockSubsidies = true)))
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe Some(OverpaymentsScheduledJourney.Features(shouldBlockSubsidies = true))

        OverpaymentsScheduledJourney.Checks.shouldBlockSubsidiesAndDeclarationHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)
      }
    }
  }
}
