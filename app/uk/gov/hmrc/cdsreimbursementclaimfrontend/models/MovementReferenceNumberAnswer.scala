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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EitherUtils._

sealed trait MovementReferenceNumberAnswer extends Product with Serializable

object MovementReferenceNumberAnswer {

  final case class IncompleteMovementReferenceNumberAnswer(
    referenceNumber: Option[Either[EntryNumber, MRN]]
  ) extends MovementReferenceNumberAnswer

  object IncompleteMovementReferenceNumberAnswer {

    val empty: IncompleteMovementReferenceNumberAnswer = IncompleteMovementReferenceNumberAnswer(None)

    implicit val format: OFormat[IncompleteMovementReferenceNumberAnswer] =
      derived.oformat[IncompleteMovementReferenceNumberAnswer]()
  }

  final case class CompleteMovementReferenceNumberAnswer(
    referenceNumber: Either[EntryNumber, MRN]
  ) extends MovementReferenceNumberAnswer

  object CompleteMovementReferenceNumberAnswer {
    implicit val format: OFormat[CompleteMovementReferenceNumberAnswer] =
      derived.oformat[CompleteMovementReferenceNumberAnswer]()
  }

  implicit class MovementReferenceNumberOps(
    private val a: MovementReferenceNumberAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteMovementReferenceNumberAnswer => A,
      ifComplete: CompleteMovementReferenceNumberAnswer => A
    ): A =
      a match {
        case i: IncompleteMovementReferenceNumberAnswer => ifIncomplete(i)
        case c: CompleteMovementReferenceNumberAnswer   => ifComplete(c)
      }
  }

  implicit val format: OFormat[MovementReferenceNumberAnswer] = derived.oformat[MovementReferenceNumberAnswer]()
}
