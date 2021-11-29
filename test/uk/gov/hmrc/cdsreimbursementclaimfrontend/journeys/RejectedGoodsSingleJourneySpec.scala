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

import RejectedGoodsSingleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class RejectedGoodsSingleJourneySpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with RejectedGoodsSingleJourneyTestData {

  val emptyJourney = RejectedGoodsSingleJourney.empty

  "RejectedGoodsSingleJourney" should {
    "have an empty instance" in {
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
      emptyJourney.answers.importerEoriNumber               shouldBe None
      emptyJourney.answers.inspectionAddress                shouldBe None
      emptyJourney.answers.inspectionDate                   shouldBe None
      emptyJourney.answers.methodOfDisposal                 shouldBe None
      emptyJourney.answers.reimbursementClaims              shouldBe None
      emptyJourney.answers.reimbursementMethodAnswer        shouldBe None
      emptyJourney.answers.supportingEvidences              shouldBe None
      emptyJourney.getNdrcDetails                           shouldBe None
      emptyJourney.getSelectedDuties                        shouldBe None
      emptyJourney.isAllSelectedDutiesAreCMAEligible        shouldBe false
      emptyJourney.isCompleteReimbursementClaims            shouldBe false
      emptyJourney.isCompleteSupportingEvidences            shouldBe false
      emptyJourney.isComplete                               shouldBe false
    }

    "check completeness of the journey" in {
      forAll(completeJourneyGen) { journey =>
        assert(journey.isComplete)
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

  }

}
