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

import java.time.LocalDate
import java.util.UUID

sealed trait DraftClaim extends Product with Serializable {
  val id: UUID
  val lastUpdatedDate: LocalDate
}

final case class DraftC285Claim(
  id: UUID,
  supportingEvidenceAnswers: Option[SupportingEvidenceAnswers],
  lastUpdatedDate: LocalDate
) extends DraftClaim

object DraftC285Claim {
  implicit val eq: Eq[DraftC285Claim] = Eq.fromUniversalEquals[DraftC285Claim]

  val newDraftC285Claim: DraftC285Claim = DraftC285Claim(UUID.randomUUID(), None, LocalDate.now)
}

object DraftClaim {

  implicit class DraftClaimOps(private val draftClaim: DraftClaim) extends AnyVal {
    def fold[A](
      draftC285Claim: DraftC285Claim => A
    ): A =
      draftClaim match {
        case a: DraftC285Claim => draftC285Claim(a)
      }
  }

  implicit val eq: Eq[DraftClaim] = Eq.fromUniversalEquals

  implicit val format: OFormat[DraftClaim] = derived.oformat()

}
