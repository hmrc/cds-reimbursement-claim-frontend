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

sealed trait ReasonForClaimAndBasisAnswer extends Product with Serializable

object ReasonForClaimAndBasisAnswer {

  final case class IncompleteReasonForClaimAndBasisAnswer(
    maybeSelectReasonForClaimAndBasis: Option[SelectReasonForClaimAndBasis]
  ) extends ReasonForClaimAndBasisAnswer

  object IncompleteReasonForClaimAndBasisAnswer {
    val empty: IncompleteReasonForClaimAndBasisAnswer                    = IncompleteReasonForClaimAndBasisAnswer(None)
    implicit val format: OFormat[IncompleteReasonForClaimAndBasisAnswer] =
      Json.format[IncompleteReasonForClaimAndBasisAnswer]
  }

  final case class CompleteReasonForClaimAndBasisAnswer(
    selectReasonForBasisAndClaim: SelectReasonForClaimAndBasis
  ) extends ReasonForClaimAndBasisAnswer

  object CompleteReasonForClaimAndBasisAnswer {
    implicit val format: OFormat[CompleteReasonForClaimAndBasisAnswer] =
      Json.format[CompleteReasonForClaimAndBasisAnswer]
  }

  implicit class ReasonForClaimAndBasisOps(
    private val a: ReasonForClaimAndBasisAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteReasonForClaimAndBasisAnswer => A,
      ifComplete: CompleteReasonForClaimAndBasisAnswer => A
    ): A =
      a match {
        case i: IncompleteReasonForClaimAndBasisAnswer => ifIncomplete(i)
        case c: CompleteReasonForClaimAndBasisAnswer   => ifComplete(c)
      }
  }

  implicit val format: OFormat[ReasonForClaimAndBasisAnswer] = derived.oformat()
}
