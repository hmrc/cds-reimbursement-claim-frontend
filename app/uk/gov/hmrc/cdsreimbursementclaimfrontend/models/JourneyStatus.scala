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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

sealed trait JourneyStatus extends Product with Serializable

object JourneyStatus {

  final case class FillingOutClaim(
    ggCredId: GGCredId,
    signedInUserDetails: SignedInUserDetails,
    draftClaim: DraftClaim
  ) extends JourneyStatus {

    def consigneeEORI: Option[String] = for {
      declaration <- draftClaim.displayDeclaration
      consignee   <- declaration.displayResponseDetail.consigneeDetails
    } yield consignee.consigneeEORI

    def declarantEORI: Option[String] = for {
      declaration <- draftClaim.displayDeclaration
    } yield declaration.displayResponseDetail.declarantDetails.declarantEORI

  }

  final case class JustSubmittedClaim(
    ggCredId: GGCredId,
    signedInUserDetails: SignedInUserDetails,
    completeClaim: CompleteClaim,
    submissionResponse: SubmitClaimResponse,
    journey: JourneyBindable
  ) extends JourneyStatus

  final case class SubmitClaimFailed(
    ggCredId: GGCredId,
    signedInUserDetails: SignedInUserDetails,
    journey: JourneyBindable
  ) extends JourneyStatus

  final case object NonGovernmentGatewayJourney extends JourneyStatus

  object FillingOutClaim {
    def of(fillingOutClaim: FillingOutClaim)(f: DraftC285Claim => DraftC285Claim): FillingOutClaim =
      fillingOutClaim.copy(draftClaim = fillingOutClaim.draftClaim.fold(f))

    def ofEither(fillingOutClaim: FillingOutClaim)(
      f: DraftC285Claim => Either[Unit, DraftC285Claim]
    ): Either[Unit, FillingOutClaim] =
      fillingOutClaim.draftClaim match {
        case draftC285Claim: DraftC285Claim =>
          f(draftC285Claim)
            .map(updatedDraftClaim => fillingOutClaim.copy(draftClaim = updatedDraftClaim))
      }
  }

  implicit val format: OFormat[JourneyStatus]                  = derived.oformat()
  implicit val fillingOutClaimFormat: OFormat[FillingOutClaim] = derived.oformat()

  implicit val eq: Eq[JourneyStatus] = Eq.fromUniversalEquals

}
