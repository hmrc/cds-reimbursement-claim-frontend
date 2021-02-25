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
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectReasonForBasisAndClaimController.SelectReasonForClaimAndBasis

sealed trait ReasonAndBasisOfClaimAnswer extends Product with Serializable

object ReasonAndBasisOfClaimAnswer {

  final case class IncompleteReasonAndBasisOfClaimAnswer(
    maybeSelectReasonForClaimAndBasis: Option[SelectReasonForClaimAndBasis]
  ) extends ReasonAndBasisOfClaimAnswer

  object IncompleteReasonAndBasisOfClaimAnswer {
    val empty: IncompleteReasonAndBasisOfClaimAnswer                    = IncompleteReasonAndBasisOfClaimAnswer(None)
    implicit val format: OFormat[IncompleteReasonAndBasisOfClaimAnswer] =
      Json.format[IncompleteReasonAndBasisOfClaimAnswer]
  }

  final case class CompleteReasonAndBasisOfClaimAnswer(
    selectReasonForBasisAndClaim: SelectReasonForClaimAndBasis
  ) extends ReasonAndBasisOfClaimAnswer

  object CompleteReasonAndBasisOfClaimAnswer {
    implicit val format: OFormat[CompleteReasonAndBasisOfClaimAnswer] =
      Json.format[CompleteReasonAndBasisOfClaimAnswer]
  }

  implicit class ReasonForClaimAndBasisOps(
    private val a: ReasonAndBasisOfClaimAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteReasonAndBasisOfClaimAnswer => A,
      ifComplete: CompleteReasonAndBasisOfClaimAnswer => A
    ): A =
      a match {
        case i: IncompleteReasonAndBasisOfClaimAnswer => ifIncomplete(i)
        case c: CompleteReasonAndBasisOfClaimAnswer   => ifComplete(c)
      }

    def reasonForClaimAndBasis: Option[SelectReasonForClaimAndBasis] = a match {
      case IncompleteReasonAndBasisOfClaimAnswer(maybeSelectReasonForClaimAndBasis) =>
        maybeSelectReasonForClaimAndBasis
      case CompleteReasonAndBasisOfClaimAnswer(selectReasonForBasisAndClaim)        => Some(selectReasonForBasisAndClaim)
    }
  }

  implicit val format: OFormat[ReasonAndBasisOfClaimAnswer] = derived.oformat()
}
