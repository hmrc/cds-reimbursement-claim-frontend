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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceAnswers

import java.time.LocalDate
import java.util.UUID

sealed trait CompleteClaim extends Product with Serializable {
  val id: UUID
  val lastUpdatedDate: LocalDate
}

final case class CompleteC285Claim(
  id: UUID,
  supportingEvidenceAnswers: Option[SupportingEvidenceAnswers],
  lastUpdatedDate: LocalDate
) extends CompleteClaim

object CompleteC285Claim {
  implicit val eq: Eq[CompleteC285Claim]          =
    Eq.fromUniversalEquals[CompleteC285Claim]
  implicit val format: OFormat[CompleteC285Claim] = Json.format[CompleteC285Claim]

}

object CompleteClaim {

  implicit val eq: Eq[CompleteClaim] = Eq.fromUniversalEquals

  implicit val format: OFormat[CompleteClaim] = derived.oformat()

}
