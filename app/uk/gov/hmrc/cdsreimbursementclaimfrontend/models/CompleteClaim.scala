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
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.CompleteMovementReferenceNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.Declaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidence
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceAnswers.CompleteSupportingEvidenceAnswers

import java.time.LocalDate
import java.util.UUID

sealed trait CompleteClaim extends Product with Serializable {
  val id: UUID
  val lastUpdatedDate: LocalDate
}

final case class CompleteC285Claim(
  id: UUID,
  movementReferenceNumberAnswer: CompleteMovementReferenceNumberAnswer,
  declaration: Option[Declaration],
  supportingEvidenceAnswers: CompleteSupportingEvidenceAnswers,
  totalClaim: String,
  lastUpdatedDate: LocalDate
) extends CompleteClaim

object CompleteC285Claim {

  //TODO: fix to bring the right data thru
  def fromDraftClaim(draftClaim: DraftClaim): Option[CompleteC285Claim] =
    draftClaim match {
      case DraftClaim.DraftC285Claim(
            id,
            None,
            Some(m: CompleteMovementReferenceNumberAnswer),
            _,
            _,
            _,
            _,
            _,
            _,
            Some(s: CompleteSupportingEvidenceAnswers),
            _,
            _,
            _,
            _,
            _,
            lastUpdatedDate
          ) =>
        Some(
          CompleteC285Claim(
            id = id,
            m,
            declaration = None,
            supportingEvidenceAnswers = s,
            totalClaim = "200",
            lastUpdatedDate = lastUpdatedDate
          )
        )
      case _ =>
        None
    }

  implicit val eq: Eq[CompleteC285Claim]          = Eq.fromUniversalEquals[CompleteC285Claim]
  implicit val format: OFormat[CompleteC285Claim] = Json.format[CompleteC285Claim]
}

object CompleteClaim {

  implicit class CompleteClaimOps(private val completeClaim: CompleteClaim) {
    def totalClaim: String = completeClaim match {
      case CompleteC285Claim(_, _, _, _, totalClaim, _) => totalClaim
    }

    def importDate: String = completeClaim match {
      case CompleteC285Claim(_, _, declaration, _, _, _) =>
        declaration match {
          case Some(value) => value.acceptanceDate
          case None        => "to be implemented"
        }
    }

    def declarantDetails: declaration.DeclarantDetails = completeClaim match {
      case CompleteC285Claim(_, _, declaration, _, _, _) =>
        declaration match {
          case Some(value) => value.declarantDetails
          case None        => //FIXME
            uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarantDetails(
              "",
              "",
              uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress(
                "",
                None,
                None,
                None,
                ""
              ),
              None
            )
        }
    }

    def declarantEmailAddress: Option[String] = declarantDetails.contactDetails.flatMap { c =>
      c.emailAddress
    }

    def declarantTelephone: Option[String] = declarantDetails.contactDetails.flatMap { c =>
      c.telephone
    }

    def supportingEvidences: List[SupportingEvidence] = completeClaim match {
      case CompleteC285Claim(_, _, _, supportingEvidenceAnswers, _, _) => supportingEvidenceAnswers.evidences
    }

    def mrn: String = completeClaim match {
      case CompleteC285Claim(_, _, declaration, _, _, _) =>
        declaration match {
          case Some(value) => value.declarantId
          case None        => "to be implemented"
        }
    }

    def declarantAddress: Option[String] = completeClaim match {
      case CompleteC285Claim(_, _, declaration, _, _, _) =>
        declaration match {
          case Some(value) =>
            value.declarantDetails.contactDetails.map { c =>
              s"${c.addressLine1.getOrElse("")}, ${c.addressLine2.getOrElse("")}, ${c.addressLine3
                .getOrElse("")}, ${c.addressLine4.getOrElse("")}, ${c.postalCode.getOrElse("")}, ${c.countryCode.getOrElse("")}"
            }

          case None => Some("") //FIXME
        }
    }

  }

  implicit val eq: Eq[CompleteClaim] = Eq.fromUniversalEquals

  implicit val format: OFormat[CompleteClaim] = derived.oformat()

}
