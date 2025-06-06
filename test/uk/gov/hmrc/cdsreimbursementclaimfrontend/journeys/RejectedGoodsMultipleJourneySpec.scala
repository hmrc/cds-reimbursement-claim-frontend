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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*

class RejectedGoodsMultipleJourneySpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  val MRNS_SIZE: Int = 5

  "RejectedGoodsMultipleJourney" should {
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
      emptyJourney.getLeadDisplayDeclaration                                      shouldBe None
      emptyJourney.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyJourney.answers.inspectionAddress                                      shouldBe None
      emptyJourney.answers.inspectionDate                                         shouldBe None
      emptyJourney.answers.methodOfDisposal                                       shouldBe None
      emptyJourney.answers.correctedAmounts                                       shouldBe None
      emptyJourney.answers.selectedDocumentType                                   shouldBe None
      emptyJourney.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyJourney.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyJourney.getNdrcDetailsFor(exampleMrn)                                  shouldBe None
      emptyJourney.getSelectedDuties(exampleMrn)                                  shouldBe None
      emptyJourney.getTotalReimbursementAmount                                    shouldBe BigDecimal("0.00")
      emptyJourney.isAllSelectedDutiesAreCMAEligible                              shouldBe false
      emptyJourney.hasCompleteReimbursementClaims                                 shouldBe false
      emptyJourney.hasCompleteSupportingEvidences                                 shouldBe false
      emptyJourney.hasCompleteAnswers                                             shouldBe false
      emptyJourney.toOutput.isLeft                                                shouldBe true
      emptyJourney.isFinalized                                                    shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeJourneyGen) { journey =>
        RejectedGoodsMultipleJourney.validator.apply(journey) shouldBe Right(())
        journey.answers.checkYourAnswersChangeMode            shouldBe true
        journey.hasCompleteReimbursementClaims                shouldBe true
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteAnswers                            shouldBe true
        journey.isFinalized                                   shouldBe false
        journey.isSubsidyOnlyJourney                          shouldBe false

        val output = journey.toOutput.getOrElse(fail("Journey output not defined."))

        output.movementReferenceNumbers shouldBe journey.answers.movementReferenceNumbers.get
        output.claimantType             shouldBe journey.getClaimantType
        output.basisOfClaim             shouldBe journey.answers.basisOfClaim.get
        output.methodOfDisposal         shouldBe journey.answers.methodOfDisposal.get
        output.detailsOfRejectedGoods   shouldBe journey.answers.detailsOfRejectedGoods.get
        output.inspectionDate           shouldBe journey.answers.inspectionDate.get
        output.inspectionAddress        shouldBe journey.answers.inspectionAddress.get
        output.reimbursementMethod      shouldBe ReimbursementMethod.BankAccountTransfer
        output.reimbursementClaims      shouldBe journey.getReimbursementClaims
        output.reimbursementClaims.size shouldBe journey.countOfMovementReferenceNumbers
        output.supportingEvidences      shouldBe journey.answers.supportingEvidences.map(EvidenceDocument.from)
        output.bankAccountDetails       shouldBe journey.answers.bankAccountDetails
        output.claimantInformation.eori shouldBe journey.answers.userEoriNumber
      }
    }

    "check completeness and produce the correct output when only subsidies" in {
      forAll(completeJourneyWithOnlySubsidiesGen) { journey =>
        RejectedGoodsMultipleJourney.validator.apply(journey) shouldBe Right(())
        journey.answers.checkYourAnswersChangeMode            shouldBe true
        journey.hasCompleteReimbursementClaims                shouldBe true
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteAnswers                            shouldBe true
        journey.isFinalized                                   shouldBe false
        journey.isSubsidyOnlyJourney                          shouldBe true

        val output = journey.toOutput.getOrElse(fail("Journey output not defined."))

        output.movementReferenceNumbers shouldBe journey.answers.movementReferenceNumbers.get
        output.claimantType             shouldBe journey.getClaimantType
        output.basisOfClaim             shouldBe journey.answers.basisOfClaim.get
        output.methodOfDisposal         shouldBe journey.answers.methodOfDisposal.get
        output.detailsOfRejectedGoods   shouldBe journey.answers.detailsOfRejectedGoods.get
        output.inspectionDate           shouldBe journey.answers.inspectionDate.get
        output.inspectionAddress        shouldBe journey.answers.inspectionAddress.get
        output.reimbursementMethod      shouldBe ReimbursementMethod.Subsidy
        output.reimbursementClaims      shouldBe journey.getReimbursementClaims
        output.reimbursementClaims.size shouldBe journey.countOfMovementReferenceNumbers
        output.supportingEvidences      shouldBe journey.answers.supportingEvidences.map(EvidenceDocument.from)
        output.bankAccountDetails       shouldBe None
        output.claimantInformation.eori shouldBe journey.answers.userEoriNumber
      }
    }

    "check incompleteness if less than two MRNs" in {
      forAll(buildCompleteJourneyGen(minNumberOfMRNs = 1, maxNumberOfMRNs = 1)) { journey =>
        RejectedGoodsMultipleJourney.validator(journey).headErrorOption shouldBe Some(
          MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER
        )
        journey.answers.checkYourAnswersChangeMode                      shouldBe false
        journey.hasCompleteReimbursementClaims                          shouldBe true
        journey.hasCompleteSupportingEvidences                          shouldBe true
        journey.hasCompleteAnswers                                      shouldBe false
        journey.isFinalized                                             shouldBe false
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

    "accept submission of a first MRN and DisplayDeclaration" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, displayDeclaration) =>
        val journey = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(mrn, displayDeclaration)
          .getOrFail
        journey.getLeadMovementReferenceNumber shouldBe Some(mrn)
        journey.getLeadDisplayDeclaration      shouldBe Some(displayDeclaration)
        journey.hasCompleteAnswers             shouldBe false
        journey.hasCompleteReimbursementClaims shouldBe false
        journey.hasCompleteSupportingEvidences shouldBe false
        journey.isFinalized                    shouldBe false
      }
    }

    "decline submission of a wrong display declaration" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, decl) =>
        val journeyEither = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(mrn, decl.withDeclarationId("foo"))

        journeyEither shouldBe Left("submitMovementReferenceNumber.wrongDisplayDeclarationMrn")
      }
    }

    "accept submission of multiple MRNs and DisplayDeclarations" in {
      def submitData(journey: RejectedGoodsMultipleJourney)(data: ((MRN, DisplayDeclaration), Int)) =
        journey.submitMovementReferenceNumberAndDeclaration(data._2, data._1._1, data._1._2)

      forAll(listOfExactlyN(11, mrnWithDisplayDeclarationGen)) { data =>
        val dataWithIndex = data.zipWithIndex
        val journey       = emptyJourney
          .flatMapEach(dataWithIndex, submitData)
          .getOrFail

        journey.countOfMovementReferenceNumbers shouldBe 11
        journey.getLeadMovementReferenceNumber  shouldBe data.headOption.map(_._1)
        dataWithIndex.foreach { case ((mrn, decl), index) =>
          journey.getNthMovementReferenceNumber(index) shouldBe Some(mrn)
          journey.getDisplayDeclarationFor(mrn)        shouldBe Some(decl)
        }

        journey.hasCompleteAnswers             shouldBe false
        journey.hasCompleteReimbursementClaims shouldBe false
        journey.hasCompleteSupportingEvidences shouldBe false
        journey.isFinalized                    shouldBe false
      }
    }

    "decline submission of an out-of-order MRN and DisplayDeclaration" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, displayDeclaration) =>
        val journeyEither = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(1, mrn, displayDeclaration)
        journeyEither shouldBe Left("submitMovementReferenceNumber.invalidIndex")
      }
    }

    "accept change of the first MRN" in {
      forAll(completeJourneyGen, mrnWithDisplayDeclarationGen) { case (journey, (mrn, displayDeclaration)) =>
        val modifiedJourney =
          journey
            .submitMovementReferenceNumberAndDeclaration(0, mrn, displayDeclaration)
            .getOrFail
        modifiedJourney.getLeadMovementReferenceNumber  shouldBe Some(mrn)
        modifiedJourney.countOfMovementReferenceNumbers shouldBe 1
        modifiedJourney.getLeadDisplayDeclaration       shouldBe Some(displayDeclaration)
        modifiedJourney.hasCompleteAnswers              shouldBe false
        modifiedJourney.hasCompleteReimbursementClaims  shouldBe false
        modifiedJourney.hasCompleteSupportingEvidences  shouldBe false
        modifiedJourney.answers.correctedAmounts        shouldBe None
        modifiedJourney.answers.inspectionAddress       shouldBe None
        modifiedJourney.answers.inspectionDate          shouldBe None
        modifiedJourney.answers.methodOfDisposal        shouldBe None
        modifiedJourney.answers.correctedAmounts        shouldBe None
        modifiedJourney.answers.selectedDocumentType    shouldBe None
        modifiedJourney.answers.supportingEvidences     shouldBe Seq.empty
      }
    }

    "accept change of the first MRN when user has XI eori" in {
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

    "accept change of the second MRN with declarantEori matching" in {
      forAll(completeJourneyGen, mrnWithDisplayDeclarationGen) { case (journey, (mrn, displayDeclaration)) =>
        val displayDeclarationWithDeclarantEoriMatching =
          displayDeclaration.withDeclarantEori(journey.getDeclarantEoriFromACC14.get)
        val modifiedJourney                             =
          journey
            .submitMovementReferenceNumberAndDeclaration(
              1,
              mrn,
              displayDeclarationWithDeclarantEoriMatching
            )
            .getOrFail
        modifiedJourney.getNthMovementReferenceNumber(1) shouldBe Some(mrn)
        modifiedJourney.countOfMovementReferenceNumbers  shouldBe journey.countOfMovementReferenceNumbers
        modifiedJourney.getNthDisplayDeclaration(1)      shouldBe Some(displayDeclarationWithDeclarantEoriMatching)
        modifiedJourney.hasCompleteReimbursementClaims   shouldBe false
        modifiedJourney.hasCompleteAnswers               shouldBe false
      }
    }

    "accept change of the second MRN with consigneeEori matching" in {
      forAll(completeJourneyGen, mrnWithDisplayDeclarationGen) { case (journey, (mrn, displayDeclaration)) =>
        val displayDeclarationWithEoriMatching = journey.getConsigneeEoriFromACC14 match {
          case Some(eori) => displayDeclaration.withConsigneeEori(eori)
          case None       => displayDeclaration.withDeclarantEori(journey.getDeclarantEoriFromACC14.get)
        }
        val modifiedJourney                    =
          journey
            .submitMovementReferenceNumberAndDeclaration(
              1,
              mrn,
              displayDeclarationWithEoriMatching
            )
            .getOrFail
        modifiedJourney.getNthMovementReferenceNumber(1) shouldBe Some(mrn)
        modifiedJourney.countOfMovementReferenceNumbers  shouldBe journey.countOfMovementReferenceNumbers
        modifiedJourney.getNthDisplayDeclaration(1)      shouldBe Some(displayDeclarationWithEoriMatching)
        modifiedJourney.hasCompleteReimbursementClaims   shouldBe false
        modifiedJourney.hasCompleteAnswers               shouldBe false
      }
    }

    "accept submission of the same first MRN" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourney = journey
          .submitMovementReferenceNumberAndDeclaration(
            0,
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

    "accept submission of the same nth MRN" in {
      forAll(completeJourneyGen) { journey =>
        journey.answers.movementReferenceNumbers.get.zipWithIndex.foreach { case (mrn, index) =>
          val modifiedJourney = journey
            .submitMovementReferenceNumberAndDeclaration(
              index,
              mrn,
              journey.getDisplayDeclarationFor(mrn).get
            )
            .getOrFail
          modifiedJourney                                shouldBe journey
          modifiedJourney.hasCompleteAnswers             shouldBe true
          modifiedJourney.hasCompleteReimbursementClaims shouldBe true
          modifiedJourney.hasCompleteSupportingEvidences shouldBe true
        }
      }
    }

    "decline removal of a first MRN" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourneyEither = journey
          .removeMovementReferenceNumberAndDisplayDeclaration(
            journey.getLeadMovementReferenceNumber.get
          )
        modifiedJourneyEither shouldBe Left("removeMovementReferenceNumberAndDisplayDeclaration.cannotRemoveFirstMRN")
      }
    }

    "accept removal of non-first MRN" in {
      forAll(buildCompleteJourneyGen(minNumberOfMRNs = 3)) { journey =>
        journey.answers.movementReferenceNumbers.get.drop(1).foreach { mrn =>
          val modifiedJourney = journey
            .removeMovementReferenceNumberAndDisplayDeclaration(mrn)
            .getOrFail
          modifiedJourney                                  should not be journey
          modifiedJourney.hasCompleteAnswers             shouldBe true
          modifiedJourney.hasCompleteReimbursementClaims shouldBe true
          modifiedJourney.hasCompleteSupportingEvidences shouldBe true
        }
      }
    }

    "decline removal of second MRN if only two" in {
      forAll(buildCompleteJourneyGen(minNumberOfMRNs = 2, maxNumberOfMRNs = 2)) { journey =>
        journey.answers.movementReferenceNumbers.get.drop(1).foreach { mrn =>
          journey
            .removeMovementReferenceNumberAndDisplayDeclaration(mrn)
            .expectFailure("removeMovementReferenceNumberAndDisplayDeclaration.cannotRemoveSecondMRN")
        }
      }
    }

    "decline removal of non-existent MRN" in {
      forAll(completeJourneyGen, IdGen.genMRN) { (journey, mrn) =>
        val modifiedJourneyEither = journey
          .removeMovementReferenceNumberAndDisplayDeclaration(mrn)
        modifiedJourneyEither shouldBe Left("removeMovementReferenceNumberAndDisplayDeclaration.notFound")
      }
    }

    "decline submission of a duplicate MRN" in {
      forAll(mrnWithDisplayDeclarationGen) { case (mrn, decl) =>
        val acc14 = decl.withDeclarationId(mrn.value)

        val journeyEither = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(0, mrn, acc14)
          .flatMap(_.submitMovementReferenceNumberAndDeclaration(1, mrn, acc14))

        journeyEither shouldBe Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
      }
    }

    "accept submission of the same nth MRN and different declaration" in {
      forAll(completeJourneyGen, displayDeclarationGen) { case (journey, declaration) =>
        val declarationWithMatchingEori = declaration.withDeclarantEori(journey.getDeclarantEoriFromACC14.get)
        journey.answers.movementReferenceNumbers.get.zipWithIndex.foreach { case (mrn, index) =>
          val modifiedJourney = journey
            .submitMovementReferenceNumberAndDeclaration(
              index,
              mrn,
              declarationWithMatchingEori.withDeclarationId(mrn.value)
            )
            .getOrFail
          modifiedJourney                                  should not be journey
          modifiedJourney.hasCompleteAnswers             shouldBe false
          modifiedJourney.hasCompleteReimbursementClaims shouldBe false
        }
      }
    }

    "reject submission of the same nth MRN and different declaration if mrn not matching" in {
      forAll(completeJourneyGen, displayDeclarationGen) { case (journey, declaration) =>
        journey.answers.movementReferenceNumbers.get.zipWithIndex.foreach { case (mrn, index) =>
          val result = journey
            .submitMovementReferenceNumberAndDeclaration(
              index,
              mrn,
              declaration
            )
          result shouldBe Left("submitMovementReferenceNumber.wrongDisplayDeclarationMrn")
        }
      }
    }

    "reject submission of the same nth MRN and different declaration if eori not matching" in {
      forAll(completeJourneyGen, displayDeclarationGen) { case (journey, declaration) =>
        journey.answers.movementReferenceNumbers.get.zipWithIndex.drop(1).foreach { case (mrn, index) =>
          val result = journey
            .submitMovementReferenceNumberAndDeclaration(
              index,
              mrn,
              declaration.withDeclarationId(mrn.value)
            )
          result shouldBe Left("submitMovementReferenceNumber.wrongDisplayDeclarationEori")
        }
      }
    }

    "decline submission of an existing MRN in different position" in {
      forAll(completeJourneyGen) { journey =>
        journey.answers.movementReferenceNumbers.get.zipWithIndex.foreach { case (_, index) =>
          val i                     = if index == 0 then journey.countOfMovementReferenceNumbers - 1 else index - 1
          val existingMrn           = journey.getNthMovementReferenceNumber(i).get
          val modifiedJourneyEither = journey
            .submitMovementReferenceNumberAndDeclaration(
              index,
              existingMrn,
              exampleDisplayDeclaration
                .withDeclarationId(existingMrn.value)
                .withDeclarantEori(journey.getDeclarantEoriFromACC14.get)
            )
          modifiedJourneyEither shouldBe Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
        }
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val journey            =
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe true
      journey.getClaimantType                          shouldBe ClaimantType.User
      journey.getClaimantEori                          shouldBe exampleEori
    }

    "needs XI eori submission if user's eori not matching those of ACC14 and ACC14 contains XI eori" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val journey            =
        RejectedGoodsMultipleJourney
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
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Declarant
      journey.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's eori is matching that of consignee" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleEori))
      val journey            =
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Consignee
      journey.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is missing" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleXIEori, consigneeEORI = None)
      val journey            =
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
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
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
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
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
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
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
          .getOrFail
          .submitConsigneeEoriNumber(anotherExampleEori)

      journeyEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "fail if submitted declarant EORI is not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val journeyEither      =
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left(JourneyValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val journey =
          RejectedGoodsMultipleJourney
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
        val journey = RejectedGoodsMultipleJourney.empty(exampleEori).submitContactAddress(contactAddress)

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
        val journey = RejectedGoodsMultipleJourney.empty(exampleEori).submitBasisOfClaim(basisOfClaim)
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
      val journeyEither = RejectedGoodsMultipleJourney
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
        val journey = RejectedGoodsMultipleJourney.empty(exampleEori).submitMethodOfDisposal(methodOfDisposal)
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
      val journey = RejectedGoodsMultipleJourney
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

    "select valid tax codes for reimbursement when none yet selected" in {
      forAll(incompleteJourneyWithMrnsGen(MRNS_SIZE)) { case (journey, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCodes         = journey.getAvailableDuties(mrn).map(_._1)
          val selectedTaxCodes = if taxCodes.size > 1 then taxCodes.drop(1) else taxCodes
          val journeyEither    = journey
            .selectAndReplaceTaxCodeSetForReimbursement(mrn, selectedTaxCodes)

          journeyEither.isRight shouldBe true
        }
      }
    }

    "replace valid tax codes for reimbursement" in {
      forAll(incompleteJourneyWithMrnsGen(MRNS_SIZE)) { case (journey, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCodes = journey.getAvailableDuties(mrn).map(_._1).sorted
          taxCodes should not be empty
          val selectedTaxCodes  = if taxCodes.size > 1 then taxCodes.drop(1) else taxCodes
          val selectedTaxCodes2 = if taxCodes.size > 1 then taxCodes.dropRight(1) else taxCodes

          val journeyEither = journey
            .selectAndReplaceTaxCodeSetForReimbursement(mrn, selectedTaxCodes)

          journeyEither.getOrFail.getSelectedDuties(mrn) shouldBe Some(selectedTaxCodes)

          val journeyEither2 =
            journeyEither.flatMap(
              _.selectAndReplaceTaxCodeSetForReimbursement(mrn, selectedTaxCodes2)
            )

          journeyEither2.getOrFail.getSelectedDuties(mrn) shouldBe Some(selectedTaxCodes2)
        }
      }

    }

    "select invalid tax codes for reimbursement" in {
      forAll(incompleteJourneyWithMrnsGen(MRNS_SIZE)) { case (journey, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val availableDuties = journey.getAvailableDuties(mrn).map(_._1).toSet
          val journeyEither   = journey
            .selectAndReplaceTaxCodeSetForReimbursement(
              mrn,
              TaxCodes.all.filter(tc => !availableDuties.contains(tc)).take(3)
            )

          journeyEither shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
        }
      }
    }

    "change tax code for reimbursement with the same set" in {
      forAll(completeJourneyGen) { journey =>
        journey.answers.movementReferenceNumbers.get.foreach { mrn =>
          val modifiedJourneyEither =
            journey.selectAndReplaceTaxCodeSetForReimbursement(mrn, journey.getSelectedDuties(mrn).get)

          val result = modifiedJourneyEither.getOrFail
          result.getSelectedDuties(mrn) shouldBe journey.getSelectedDuties(mrn)
          result.hasCompleteAnswers     shouldBe true
        }
      }
    }

    "change tax code for reimbursement with a new valid set" in {
      forAll(incompleteJourneyWithMrnsGen(MRNS_SIZE)) { case (journey, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCodes                    = journey.getAvailableDuties(mrn).map(_._1).sorted
          val newTaxCodeSet: Seq[TaxCode] = taxCodes.take(2)

          val modifiedJourneyEither = journey.selectAndReplaceTaxCodeSetForReimbursement(mrn, newTaxCodeSet)

          val result = modifiedJourneyEither.getOrFail
          result.getSelectedDuties(mrn).get shouldBe newTaxCodeSet
        }
      }
    }

    "change tax code for reimbursement with a new invalid set" in {
      forAll(incompleteJourneyWithMrnsGen(MRNS_SIZE)) { case (journey, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val invalidTaxCodeSet     = TaxCodes.all.takeRight(6)
          val modifiedJourneyEither = journey.selectAndReplaceTaxCodeSetForReimbursement(mrn, invalidTaxCodeSet)
          modifiedJourneyEither shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
        }
      }
    }

    "submit valid correct amounts for selected tax codes" in {
      def submitData(mrn: MRN)(journey: RejectedGoodsMultipleJourney)(data: (TaxCode, BigDecimal)) =
        journey.submitCorrectAmount(mrn, data._1, data._2)

      forAll(incompleteJourneyWithMrnsGen(MRNS_SIZE)) { case (journey, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCodes            = journey.getAvailableDuties(mrn).map(_._1)
          val paidAmounts         = journey.getNdrcDetailsFor(mrn).map(_.map(_.amount).map(BigDecimal.apply)).getOrElse(Nil)
          val correctAmounts      = paidAmounts.map(_ / 2)
          val taxCodesWithAmounts = taxCodes.zip(correctAmounts)
          val modifiedJourney     = journey
            .selectAndReplaceTaxCodeSetForReimbursement(mrn, taxCodes)
            .flatMapEach(taxCodesWithAmounts, submitData(mrn))
            .getOrFail

          modifiedJourney.hasCompleteAnswers                  shouldBe false
          modifiedJourney.getTotalReimbursementAmountFor(mrn) shouldBe Some(correctAmounts.sum)
          modifiedJourney.getTotalReimbursementAmount         shouldBe correctAmounts.sum
        }
      }
    }

    "submit valid claim amounts for selected tax codes" in {
      def submitData(mrn: MRN)(journey: RejectedGoodsMultipleJourney)(data: (TaxCode, BigDecimal)) =
        journey.submitClaimAmount(mrn, data._1, data._2)

      forAll(incompleteJourneyWithMrnsGen(MRNS_SIZE)) { case (journey, mrns) =>
        mrns.size shouldBe MRNS_SIZE

        mrns.foreach { mrn =>
          val taxCodes            = journey.getAvailableDuties(mrn).map(_._1)
          val paidAmounts         = journey.getNdrcDetailsFor(mrn).map(_.map(_.amount).map(BigDecimal.apply)).getOrElse(Nil)
          val claimAmounts        = paidAmounts.map(a => a / 4)
          val taxCodesWithAmounts = taxCodes.zip(claimAmounts)
          val modifiedJourney     = journey
            .selectAndReplaceTaxCodeSetForReimbursement(mrn, taxCodes)
            .flatMapEach(taxCodesWithAmounts, submitData(mrn))
            .getOrFail

          modifiedJourney.hasCompleteAnswers                  shouldBe false
          modifiedJourney.getTotalReimbursementAmountFor(mrn) shouldBe Some(claimAmounts.sum)
          modifiedJourney.getTotalReimbursementAmount         shouldBe claimAmounts.sum
        }
      }
    }

    "submit valid correct amount for wrong tax code" in {
      forAll(incompleteJourneyWithMrnsGen(MRNS_SIZE)) { case (journey, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCodes      = journey.getAvailableDuties(mrn).map(_._1)
          val wrongTaxCode  = TaxCodes.all.find(tc => !taxCodes.contains(tc)).get
          val journeyEither = journey
            .selectAndReplaceTaxCodeSetForReimbursement(mrn, taxCodes)
            .flatMap(_.submitCorrectAmount(mrn, wrongTaxCode, BigDecimal("5.00")))

          journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
        }
      }
    }

    "submit invalid correct amount for the valid tax code" in {
      forAll(incompleteJourneyWithMrnsGen(MRNS_SIZE)) { case (journey, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCode       = journey.getAvailableDuties(mrn).map(_._1).head
          val paidAmount    = journey.getNdrcDetailsFor(mrn).map(_.map(_.amount).map(BigDecimal.apply)).getOrElse(Nil).head
          val journeyEither = journey.selectAndReplaceTaxCodeSetForReimbursement(mrn, Seq(taxCode))

          val journeyEitherTestNegative =
            journeyEither.flatMap(_.submitCorrectAmount(mrn, taxCode, BigDecimal("-10.00")))
          val journeyEitherTestGreater  =
            journeyEither.flatMap(_.submitCorrectAmount(mrn, taxCode, paidAmount + 1))
          val journeyEitherTestSame     =
            journeyEither.flatMap(_.submitCorrectAmount(mrn, taxCode, paidAmount))

          journeyEitherTestNegative shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          journeyEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          journeyEitherTestSame     shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
        }
      }
    }

    "submit invalid correct amount for wrong tax code" in {
      val displayDeclaration = buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val journeyEither      = RejectedGoodsMultipleJourney
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A80, BigDecimal("0.00")))

      journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "change to valid correct amount for selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        journey.answers.movementReferenceNumbers.get.foreach { mrn =>
          val totalAmount: BigDecimal              = journey.getTotalReimbursementAmount
          val taxCodes: Seq[(TaxCode, BigDecimal)] = journey.getReimbursementClaims(mrn).toSeq
          for (taxCode, reimbursementAmount) <- taxCodes do {
            val paidAmount         = journey.getAmountPaidFor(mrn, taxCode).get
            val newCorrectedAmount = (paidAmount - reimbursementAmount) / 2
            val journeyEither      = journey.submitCorrectAmount(mrn, taxCode, newCorrectedAmount)
            val modifiedJourney    = journeyEither.getOrFail
            modifiedJourney.getTotalReimbursementAmount shouldBe (totalAmount + newCorrectedAmount)
          }
        }
      }
    }

    "change to invalid correct amount for selected tax code" in {
      forAll(completeJourneyGen) { journey =>
        val leadMrn                              = journey.getLeadMovementReferenceNumber.get
        val taxCodes: Seq[(TaxCode, BigDecimal)] = journey.getReimbursementClaims(leadMrn).toSeq
        for (taxCode, _) <- taxCodes do {
          val newAmount     = BigDecimal("-10.00")
          val journeyEither = journey.submitCorrectAmount(leadMrn, taxCode, newAmount)

          journeyEither shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
        }
      }
    }

    "change to valid correct amount for the tax code not in ACC14" in {
      forAll(completeJourneyGen) { journey =>
        val leadMrn       = journey.getLeadMovementReferenceNumber.get
        val taxCodeSet    =
          journey.getNdrcDetailsFor(leadMrn).map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val wrongTaxCode  = TaxCodes.all.find(taxCode => !taxCodeSet.contains(taxCode)).getOrElse(TaxCode.NI633)
        val journeyEither = journey.submitCorrectAmount(leadMrn, wrongTaxCode, BigDecimal("10.00"))
        journeyEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
      }
    }

    "submit inspection date" in {
      forAll(DateGen.genDate) { inspectionDate =>
        val journey = RejectedGoodsMultipleJourney.empty(exampleEori).submitInspectionDate(inspectionDate)

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
        val journey = RejectedGoodsMultipleJourney.empty(exampleEori).submitInspectionAddress(inspectionAddress)

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

    "submit bankAccountDetails and bankAccountType if reimbursement method is BankAccountTransfer" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, displayDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("0.50")))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.Business))

      journeyEither.isRight shouldBe true
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

        val journey = RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe None

        RejectedGoodsMultipleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Valid
      }

      "BlockSubsidies feature enabled and SubsidyOnlyPayments not" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = RejectedGoodsMultipleJourney
          .empty(
            exampleEori,
            features = Some(
              RejectedGoodsMultipleJourney
                .Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = false)
            )
          )
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe Some(
          RejectedGoodsMultipleJourney.Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = false)
        )

        RejectedGoodsMultipleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)
      }

      "BlockSubsidies feature disabled and SubsidyOnlyPayments enabled" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = RejectedGoodsMultipleJourney
          .empty(
            exampleEori,
            features = Some(
              RejectedGoodsMultipleJourney
                .Features(shouldBlockSubsidies = false, shouldAllowSubsidyOnlyPayments = true)
            )
          )
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe Some(
          RejectedGoodsMultipleJourney.Features(shouldBlockSubsidies = false, shouldAllowSubsidyOnlyPayments = true)
        )

        RejectedGoodsMultipleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Valid
      }

      "both BlockSubsidies and SubsidyOnlyPayments features enabled" in new DeclarationSupport {
        val declaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
            .withSomeSubsidiesPaymentMethod()

        val journey = RejectedGoodsMultipleJourney
          .empty(
            exampleEori,
            features = Some(
              RejectedGoodsMultipleJourney.Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = true)
            )
          )
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
          .getOrFail

        journey.features shouldBe Some(
          RejectedGoodsMultipleJourney.Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = true)
        )

        RejectedGoodsMultipleJourney.Checks.whenBlockSubsidiesThenDeclarationsHasNoSubsidyPayments.apply(
          journey
        ) shouldBe Validator.Valid
      }
    }
  }
}
