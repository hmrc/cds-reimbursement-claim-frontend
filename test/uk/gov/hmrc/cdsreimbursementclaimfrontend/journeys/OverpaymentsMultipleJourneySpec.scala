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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyValidationErrors._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._

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
        modifiedJourney.answers.displayDeclarations.flatMap(_.headOption) shouldBe Some(decl2)
        modifiedJourney.hasCompleteAnswers                                shouldBe false
        modifiedJourney.hasCompleteReimbursementClaims                    shouldBe false
        modifiedJourney.hasCompleteSupportingEvidences                    shouldBe false
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

      journeyEither shouldBe Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
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

      journeyEither shouldBe Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
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
        for (mrn <- journey.getMovementReferenceNumbers.get) {
          val totalAmount: BigDecimal              = journey.getReimbursementAmountForDeclaration(mrn)
          val taxCodes: Seq[(TaxCode, BigDecimal)] = journey.getReimbursementClaimsFor(mrn).toSeq
          for ((taxCode, reimbursementAmount) <- taxCodes) {
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
        for ((taxCode, amount) <- taxCodes) {
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
        for (mrn <- journey.getMovementReferenceNumbers.get) {
          val totalAmount: BigDecimal              = journey.getReimbursementAmountForDeclaration(mrn)
          val taxCodes: Seq[(TaxCode, BigDecimal)] = journey.getReimbursementClaimsFor(mrn).toSeq
          for ((taxCode, reimbursementAmount) <- taxCodes) {
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
        for ((taxCode, amount) <- taxCodes) {
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

    "getNextNdrcDetailsToClaim" when {
      "return the next Ndrc Details to claim" in {
        forAll(displayDeclarationGen, Acc14Gen.genListNdrcDetails()) {
          (displayDeclaration: DisplayDeclaration, ndrcDetails: List[NdrcDetails]) =>
            whenever(ndrcDetails.size > 1 && ndrcDetails.map(_.taxType).toSet.size == ndrcDetails.size) {
              val taxCodes             = ndrcDetails.map(details => TaxCode(details.taxType))
              val drd                  = displayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
              val updatedDd            = displayDeclaration.copy(displayResponseDetail = drd)
              val journey              = RejectedGoodsMultipleJourney
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, updatedDd)
                .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, taxCodes))
                .getOrFail
              val claimedReimbursement = journey.getCorrectAmountsFor(exampleMrn).get
              val nextDetails          = journey.getNextNdrcDetailsToClaim(exampleMrn).get
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

    "validate if any subsidy payment method is in the declaration" when {

      import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport

      "BlockSubsidies feature not enabled" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = OverpaymentsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe None

        OverpaymentsMultipleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Valid
      }

      "BlockSubsidies feature enabled and SubsidyOnlyPayments not" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = OverpaymentsMultipleJourney
          .empty(
            exampleEori,
            features = Some(
              OverpaymentsMultipleJourney
                .Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = false)
            )
          )
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe Some(
          OverpaymentsMultipleJourney.Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = false)
        )

        OverpaymentsMultipleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)
      }

      "BlockSubsidies feature disabled and SubsidyOnlyPayments enabled" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = OverpaymentsMultipleJourney
          .empty(
            exampleEori,
            features = Some(
              OverpaymentsMultipleJourney
                .Features(shouldBlockSubsidies = false, shouldAllowSubsidyOnlyPayments = true)
            )
          )
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe Some(
          OverpaymentsMultipleJourney.Features(shouldBlockSubsidies = false, shouldAllowSubsidyOnlyPayments = true)
        )

        OverpaymentsMultipleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Valid
      }

      "both BlockSubsidies and SubsidyOnlyPayments features enabled" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = OverpaymentsMultipleJourney
          .empty(
            exampleEori,
            features = Some(
              OverpaymentsMultipleJourney.Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = true)
            )
          )
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe Some(
          OverpaymentsMultipleJourney.Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = true)
        )

        OverpaymentsMultipleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Valid
      }
    }
  }
}
