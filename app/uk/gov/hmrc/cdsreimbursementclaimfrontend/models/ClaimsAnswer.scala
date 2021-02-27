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

import cats.kernel.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait ClaimsAnswer extends Product with Serializable

object ClaimsAnswer {

  final case class IncompleteClaimsAnswer(
    claims: List[Claim]
  ) extends ClaimsAnswer

  object IncompleteClaimsAnswer {
    val empty: IncompleteClaimsAnswer           = IncompleteClaimsAnswer(List.empty)
    implicit val eq: Eq[IncompleteClaimsAnswer] = Eq.fromUniversalEquals[IncompleteClaimsAnswer]

    implicit val format: OFormat[IncompleteClaimsAnswer] =
      derived.oformat[IncompleteClaimsAnswer]()
  }

  final case class CompleteClaimsAnswer(
    claims: List[Claim]
  ) extends ClaimsAnswer

  object CompleteClaimsAnswer {
    implicit val eq: Eq[CompleteClaimsAnswer]          = Eq.fromUniversalEquals[CompleteClaimsAnswer]
    implicit val format: OFormat[CompleteClaimsAnswer] =
      derived.oformat[CompleteClaimsAnswer]()
  }

  implicit class ClaimAnswersOps(
    private val a: ClaimsAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteClaimsAnswer => A,
      ifComplete: CompleteClaimsAnswer => A
    ): A =
      a match {
        case i: IncompleteClaimsAnswer => ifIncomplete(i)
        case c: CompleteClaimsAnswer   => ifComplete(c)
      }
  }

  implicit val eq: Eq[ClaimsAnswer]          = Eq.fromUniversalEquals[ClaimsAnswer]
  implicit val format: OFormat[ClaimsAnswer] = derived.oformat[ClaimsAnswer]()
}
