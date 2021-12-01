/*
 * Copyright 2021 HM Revenue & Customs
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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen

import RejectedGoodsSingleJourneyGenerators._

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class RejectedGoodsSingleJourneySpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with RejectedGoodsSingleJourneyTestData {

  "RejectedGoodsSingleJourney" should {
    "have an empty instance" in {
      emptyJourney.answers.userEoriNumber                   shouldBe exampleEori
      emptyJourney.answers.bankAccountDetails               shouldBe None
      emptyJourney.answers.bankAccountType                  shouldBe None
      emptyJourney.answers.basisOfClaim                     shouldBe None
      emptyJourney.answers.basisOfClaimSpecialCircumstances shouldBe None
      emptyJourney.answers.contactAddress                   shouldBe None
      emptyJourney.answers.contactDetails                   shouldBe None
      emptyJourney.answers.contactAddress                   shouldBe None
      emptyJourney.answers.declarantEoriNumber              shouldBe None
      emptyJourney.answers.declarantType                    shouldBe None
      emptyJourney.answers.detailsOfRejectedGoods           shouldBe None
      emptyJourney.answers.displayDeclaration               shouldBe None
      emptyJourney.answers.consigneeEoriNumber              shouldBe None
      emptyJourney.answers.inspectionAddress                shouldBe None
      emptyJourney.answers.inspectionDate                   shouldBe None
      emptyJourney.answers.methodOfDisposal                 shouldBe None
      emptyJourney.answers.reimbursementClaims              shouldBe None
      emptyJourney.answers.reimbursementMethod              shouldBe None
      emptyJourney.answers.supportingEvidences              shouldBe None
      emptyJourney.getNdrcDetails                           shouldBe None
      emptyJourney.getSelectedDuties                        shouldBe None
      emptyJourney.isAllSelectedDutiesAreCMAEligible        shouldBe false
      emptyJourney.isCompleteReimbursementClaims            shouldBe false
      emptyJourney.isCompleteSupportingEvidences            shouldBe false
      emptyJourney.isComplete                               shouldBe false
      emptyJourney.toOutput.isLeft                          shouldBe true
    }

    "check completeness and produce the correct output" in {
      forAll(completeJourneyGen) { journey =>
        journey.isComplete shouldBe true
        val output = journey.toOutput.getOrElse(fail("Journey output not defined."))
        output.movementReferenceNumber                   shouldBe journey.answers.movementReferenceNumber.get
        output.declarantType                             shouldBe journey.answers.declarantType.get
        output.basisOfClaim                              shouldBe journey.answers.basisOfClaim.get
        output.methodOfDisposal                          shouldBe journey.answers.methodOfDisposal.get
        output.detailsOfRejectedGoods                    shouldBe journey.answers.detailsOfRejectedGoods.get
        output.inspectionDate                            shouldBe journey.answers.inspectionDate.get
        output.inspectionAddress                         shouldBe journey.answers.inspectionAddress.get
        output.reimbursementMethod                       shouldBe journey.answers.reimbursementMethod
        output.reimbursementClaims                       shouldBe journey.answers.reimbursementClaims.get.mapValues(_.get)
        output.supportingEvidences                       shouldBe journey.answers.supportingEvidences.get.mapValues(_.get)
        output.consigneeAndDeclarantEoriNumber.isDefined shouldBe (journey.answers.consigneeEoriNumber.isDefined && journey.answers.declarantEoriNumber.isDefined)
        output.contactDetailsAndAddress.isDefined        shouldBe (journey.answers.contactDetails.isDefined && journey.answers.contactAddress.isDefined)
        output.bankAccountDetailsAndType.isDefined       shouldBe (journey.answers.bankAccountDetails.isDefined && journey.answers.bankAccountType.isDefined)
      }
    }

    "accept submission of a new MRN" in {
      forAll(IdGen.genMRN) { mrn =>
        val journey = emptyJourney.submitMovementReferenceNumber(mrn)
        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.isComplete                                    shouldBe false
        journey.isCompleteReimbursementClaims                 shouldBe false
        journey.isCompleteSupportingEvidences                 shouldBe false
      }
    }

    "accept change of the MRN" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourney = journey.submitMovementReferenceNumber(exampleMrn)
        modifiedJourney.answers.displayDeclaration    shouldBe empty
        modifiedJourney.isComplete                    shouldBe false
        modifiedJourney.isCompleteReimbursementClaims shouldBe false
        modifiedJourney.isCompleteSupportingEvidences shouldBe false
      }
    }

    "accept submission of the same MRN" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourney = journey
          .submitMovementReferenceNumber(journey.answers.movementReferenceNumber.get)
        modifiedJourney                               shouldBe journey
        modifiedJourney.isComplete                    shouldBe true
        modifiedJourney.isCompleteReimbursementClaims shouldBe true
        modifiedJourney.isCompleteSupportingEvidences shouldBe true
      }
    }

    "accept submission of a new ACC14 data" in {
      forAll(displayDeclarationGen) { acc14 =>
        val journey = emptyJourney
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(acc14)

        journey.answers.movementReferenceNumber.contains(exampleMrn) shouldBe true
        journey.answers.displayDeclaration.contains(acc14)           shouldBe true
        journey.isComplete                                           shouldBe false
        journey.isCompleteReimbursementClaims                        shouldBe false
        journey.isCompleteSupportingEvidences                        shouldBe false
      }
    }

    "accept change of the ACC14 data" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourney =
          journey
            .submitDisplayDeclaration(exampleDisplayDeclaration)
        modifiedJourney.answers.displayDeclaration    shouldBe Some(exampleDisplayDeclaration)
        modifiedJourney.isComplete                    shouldBe false
        modifiedJourney.isCompleteReimbursementClaims shouldBe false
        modifiedJourney.isCompleteSupportingEvidences shouldBe true
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val journey            =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe true
    }

    "does not need declarant and consignee submission if user's eori is matching that of declarant" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori, consigneeEORI = None)
      val journey            =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
    }

    "does not need declarant and consignee submission if user's eori is matching that of consignee" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleEori))
      val journey            =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
    }

    "fail building journey if user's eori not matching those of ACC14 and separate EORIs were not provided by the user" in {
      val journeyGen = buildJourneyGen(
        acc14DeclarantMatchesUserEori = false,
        acc14ConsigneeMatchesUserEori = false,
        submitDeclarantDetails = false,
        submitConsigneeDetails = false
      )
      forAll(journeyGen) { result =>
        val journey = result.getOrElse(fail("Journey building has failed."))
        journey.isComplete shouldBe false
      }
    }

    "fail if submitting consignee EORI if not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val journeyEither      =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)
          .submitConsigneeEoriNumber(anotherExampleEori)

      journeyEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitting declarant EORI if not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val journeyEither      =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)
          .submitDeclarantEoriNumber(anotherExampleEori)

      journeyEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

  }

}
