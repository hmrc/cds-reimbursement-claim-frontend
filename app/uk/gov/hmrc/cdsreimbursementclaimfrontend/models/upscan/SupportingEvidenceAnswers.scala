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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait SupportingEvidenceAnswers extends Product with Serializable

object SupportingEvidenceAnswers {

  final case class IncompleteSupportingEvidenceAnswers(
    evidences: List[SupportingEvidence]
  ) extends SupportingEvidenceAnswers

  object IncompleteSupportingEvidenceAnswers {
    val empty: IncompleteSupportingEvidenceAnswers =
      IncompleteSupportingEvidenceAnswers(List.empty)
  }

  final case class CompleteSupportingEvidenceAnswers(
    evidences: List[SupportingEvidence]
  ) extends SupportingEvidenceAnswers

  implicit class UploadSupportingDocumentsOps(
    private val a: SupportingEvidenceAnswers
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteSupportingEvidenceAnswers => A,
      ifComplete: CompleteSupportingEvidenceAnswers => A
    ): A =
      a match {
        case i: IncompleteSupportingEvidenceAnswers => ifIncomplete(i)
        case c: CompleteSupportingEvidenceAnswers   => ifComplete(c)
      }
  }

  implicit val format: OFormat[SupportingEvidenceAnswers] = derived.oformat()
}
