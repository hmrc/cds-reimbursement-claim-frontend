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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

sealed trait JourneyStatus extends Product with Serializable

object JourneyStatus {

  final case class FillingOutClaim(
    ggCredId: GGCredId,
    signedInUserDetails: SignedInUserDetails,
    draftClaim: DraftClaim
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
    def of(source: FillingOutClaim)(f: DraftC285Claim => DraftC285Claim): FillingOutClaim =
      source.copy(draftClaim = source.draftClaim.fold(f))
  }

  implicit val format: OFormat[JourneyStatus] = derived.oformat()

  implicit val eq: Eq[JourneyStatus] = Eq.fromUniversalEquals

}
