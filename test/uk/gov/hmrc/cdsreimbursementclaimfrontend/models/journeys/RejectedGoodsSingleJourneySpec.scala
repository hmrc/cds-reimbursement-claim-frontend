package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journeys
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest

class RejectedGoodsSingleJourneySpec
    extends AnyWordSpec
    with JsonFormatTest
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

    "accept new MRN submission" in {
      val journey = emptyJourney.submitMovementReferenceNumber(MRN("foo"))
      journey.answers.movementReferenceNumber.contains(MRN("foo")) shouldBe true
      journey.isComplete                                           shouldBe false
      journey.isCompleteReimbursementClaims                        shouldBe false
      journey.isCompleteSupportingEvidences                        shouldBe false
    }
  }

}
