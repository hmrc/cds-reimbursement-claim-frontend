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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDetailsRegisteredWithCdsController.ClaimantDetailsAsIndividual

sealed trait ClaimantDetailsAsIndividualAnswer extends Product with Serializable

object ClaimantDetailsAsIndividualAnswer {

  final case class IncompleteClaimantDetailsAsIndividualAnswer(
    claimantDetailsAsIndividual: Option[ClaimantDetailsAsIndividual]
  ) extends ClaimantDetailsAsIndividualAnswer

  object IncompleteClaimantDetailsAsIndividualAnswer {
    val empty: IncompleteClaimantDetailsAsIndividualAnswer = IncompleteClaimantDetailsAsIndividualAnswer(None)

    implicit val format: OFormat[IncompleteClaimantDetailsAsIndividualAnswer] =
      derived.oformat[IncompleteClaimantDetailsAsIndividualAnswer]()
  }

  final case class CompleteClaimantDetailsAsIndividualAnswer(
    claimantDetailsAsIndividual: ClaimantDetailsAsIndividual
  ) extends ClaimantDetailsAsIndividualAnswer

  object CompleteClaimantDetailsAsIndividualAnswer {
    implicit val format: OFormat[CompleteClaimantDetailsAsIndividualAnswer] =
      derived.oformat[CompleteClaimantDetailsAsIndividualAnswer]()
  }

  implicit class ClaimantDetailsAsIndividualAnswerOps(
    private val a: ClaimantDetailsAsIndividualAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteClaimantDetailsAsIndividualAnswer => A,
      ifComplete: CompleteClaimantDetailsAsIndividualAnswer => A
    ): A =
      a match {
        case i: IncompleteClaimantDetailsAsIndividualAnswer => ifIncomplete(i)
        case c: CompleteClaimantDetailsAsIndividualAnswer   => ifComplete(c)
      }
  }

  implicit val format: OFormat[ClaimantDetailsAsIndividualAnswer] = derived.oformat[ClaimantDetailsAsIndividualAnswer]()
}
