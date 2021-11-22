package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

trait C285JourneySessionFixtures {
  this: SessionSupport with ScalaCheckPropertyChecks with OptionValues with EitherValues =>

  def sessionWithCompleteC285Claim(
    mrn: MRN,
    typeOfClaim: TypeOfClaimAnswer
  ): (SessionData, JourneyStatus.FillingOutClaim) = {
    val draftC285Claim      =
      sample(DraftClaimGen.genValidDraftClaim(typeOfClaim))
        .copy(
          movementReferenceNumber = Some(mrn)
        )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = JourneyStatus.FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey
    )
  }

  def sessionWithMRNAndTypeOfClaimOnly(
    maybeMovementReferenceNumberAnswer: Option[MRN],
    typeOfClaim: Option[TypeOfClaimAnswer]
  ): (SessionData, JourneyStatus.FillingOutClaim) = {
    val draftC285Claim      =
      DraftClaim.blank.copy(
        movementReferenceNumber = maybeMovementReferenceNumberAnswer,
        typeOfClaim = typeOfClaim
      )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = JourneyStatus.FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey
    )
  }

}
