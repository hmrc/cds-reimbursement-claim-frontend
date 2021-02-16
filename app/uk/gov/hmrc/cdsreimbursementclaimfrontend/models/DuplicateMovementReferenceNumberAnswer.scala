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

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EitherUtils._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}

sealed trait DuplicateMovementReferenceNumberAnswer extends Product with Serializable

object DuplicateMovementReferenceNumberAnswer {

  final case class IncompleteDuplicateMovementReferenceNumberAnswer(
    duplicateMovementReferenceNumber: Option[Either[EntryNumber, MRN]]
  ) extends DuplicateMovementReferenceNumberAnswer

  object IncompleteDuplicateMovementReferenceNumberAnswer {
    val empty: IncompleteDuplicateMovementReferenceNumberAnswer =
      IncompleteDuplicateMovementReferenceNumberAnswer(None)

    implicit val format: OFormat[IncompleteDuplicateMovementReferenceNumberAnswer] =
      derived.oformat[IncompleteDuplicateMovementReferenceNumberAnswer]()
  }

  final case class CompleteDuplicateMovementReferenceNumberAnswer(
    duplicateMovementReferenceNumber: Option[Either[EntryNumber, MRN]]
  ) extends DuplicateMovementReferenceNumberAnswer

  object CompleteDuplicateMovementReferenceNumberAnswer {
    implicit val format: OFormat[CompleteDuplicateMovementReferenceNumberAnswer] =
      derived.oformat[CompleteDuplicateMovementReferenceNumberAnswer]()
  }

  implicit class DuplicateMovementReferenceNumberOps(
    private val a: DuplicateMovementReferenceNumberAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteDuplicateMovementReferenceNumberAnswer => A,
      ifComplete: CompleteDuplicateMovementReferenceNumberAnswer => A
    ): A =
      a match {
        case i: IncompleteDuplicateMovementReferenceNumberAnswer => ifIncomplete(i)
        case c: CompleteDuplicateMovementReferenceNumberAnswer   => ifComplete(c)
      }
  }

  implicit val format: OFormat[DuplicateMovementReferenceNumberAnswer] =
    derived.oformat[DuplicateMovementReferenceNumberAnswer]()
}
