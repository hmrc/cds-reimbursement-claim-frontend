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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AssociatedMRNsAnswer
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

  def sessionWithMRNTypeOfClaimAndAssociatedMRNs(
    maybeMovementReferenceNumberAnswer: Option[MRN],
    typeOfClaim: Option[TypeOfClaimAnswer],
    associatedMRNsAnswer: AssociatedMRNsAnswer
  ): (SessionData, JourneyStatus.FillingOutClaim) = {
    val draftC285Claim      =
      DraftClaim.blank.copy(
        movementReferenceNumber = maybeMovementReferenceNumberAnswer,
        typeOfClaim = typeOfClaim,
        associatedMRNsAnswer = Some(associatedMRNsAnswer)
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
