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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CompleteClaim.CompleteC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim.newDraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journey.ClaimType

sealed trait JourneyStatus extends Product with Serializable

object JourneyStatus {

  final case class PreFillingOutClaim(
    ggCredId: GGCredId,
    signedInUserDetails: SignedInUserDetails,
    claimType: Option[ClaimType] = None
  ) extends JourneyStatus {

    def toFillingOutClaim(
      claimType: ClaimType,
      draft: DraftClaim = newDraftC285Claim
    ): FillingOutClaim =
      FillingOutClaim(
        ggCredId,
        signedInUserDetails,
        draft,
        claimType
      )
  }

  final case class FillingOutClaim(
    ggCredId: GGCredId,
    signedInUserDetails: SignedInUserDetails,
    draftClaim: DraftClaim,
    claimType: ClaimType
  ) extends JourneyStatus

  final case class CompletedFillingOutClaim(
    ggCredId: GGCredId,
    signedInUserDetails: SignedInUserDetails,
    completeClaim: CompleteClaim,
    claimType: ClaimType
  ) extends JourneyStatus

  final case class JustSubmittedClaim(
    ggCredId: GGCredId,
    signedInUserDetails: SignedInUserDetails,
    completeClaim: CompleteClaim,
    submissionResponse: SubmitClaimResponse
  ) extends JourneyStatus

  final case class SubmitClaimFailed(
    ggCredId: GGCredId,
    signedInUserDetails: SignedInUserDetails
  ) extends JourneyStatus

  final case object NonGovernmentGatewayJourney extends JourneyStatus

  object FillingOutClaim {
    def of(fillingOutClaim: FillingOutClaim)(f: DraftC285Claim => DraftC285Claim): FillingOutClaim =
      fillingOutClaim.copy(draftClaim = fillingOutClaim.draftClaim.fold(f))
  }

  object CompletedFillingOutClaim {
    def of(completedFillingOutClaim: CompletedFillingOutClaim)(
      f: CompleteC285Claim => CompleteC285Claim
    ): CompletedFillingOutClaim =
      completedFillingOutClaim.copy(completeClaim = completedFillingOutClaim.completeClaim.fold(f))
  }

  implicit val format: OFormat[JourneyStatus] = derived.oformat[JourneyStatus]()

  implicit val eq: Eq[JourneyStatus] = Eq.fromUniversalEquals

}
