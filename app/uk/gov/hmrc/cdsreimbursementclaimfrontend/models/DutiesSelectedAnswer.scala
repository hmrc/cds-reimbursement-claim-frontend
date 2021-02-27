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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form.DutiesSelected

sealed trait DutiesSelectedAnswer extends Product with Serializable

object DutiesSelectedAnswer {

  final case class IncompleteDutiesSelectedAnswer(
    maybeDutiesSelected: Option[DutiesSelected]
  ) extends DutiesSelectedAnswer

  object IncompleteDutiesSelectedAnswer {
    val empty: IncompleteDutiesSelectedAnswer           = IncompleteDutiesSelectedAnswer(None)
    implicit val eq: Eq[IncompleteDutiesSelectedAnswer] = Eq.fromUniversalEquals[IncompleteDutiesSelectedAnswer]

    implicit val format: OFormat[IncompleteDutiesSelectedAnswer] =
      derived.oformat[IncompleteDutiesSelectedAnswer]()
  }

  final case class CompleteDutiesSelectedAnswer(
    dutiesSelected: DutiesSelected
  ) extends DutiesSelectedAnswer

  object CompleteDutiesSelectedAnswer {
    implicit val format: OFormat[CompleteDutiesSelectedAnswer] =
      derived.oformat[CompleteDutiesSelectedAnswer]()
  }

  implicit class DutiesToClaimAgainstAnswerOps(
    private val a: DutiesSelectedAnswer
  ) extends AnyVal {
    def fold[A](
      ifIncomplete: IncompleteDutiesSelectedAnswer => A,
      ifComplete: CompleteDutiesSelectedAnswer => A
    ): A =
      a match {
        case i: IncompleteDutiesSelectedAnswer => ifIncomplete(i)
        case c: CompleteDutiesSelectedAnswer   => ifComplete(c)
      }
  }

  implicit val eq: Eq[DutiesSelectedAnswer] = Eq.fromUniversalEquals[DutiesSelectedAnswer]

  implicit val format: OFormat[DutiesSelectedAnswer] =
    derived.oformat[DutiesSelectedAnswer]()
}
