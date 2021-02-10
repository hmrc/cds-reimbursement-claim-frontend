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

sealed trait ReasonForClaimAnswer extends Product with Serializable

object ReasonForClaimAnswer {

  final case class IncompleteReasonForClaimAnswer(
    reasonForClaimOption: Option[ReasonForClaimOption]
  ) extends ReasonForClaimAnswer

  object IncompleteReasonForClaimAnswer {
    val empty: IncompleteReasonForClaimAnswer                    = IncompleteReasonForClaimAnswer(None)
    implicit val format: OFormat[IncompleteReasonForClaimAnswer] = Json.format[IncompleteReasonForClaimAnswer]
  }

  final case class CompleteReasonForClaimAnswer(
    reasonForClaimOption: ReasonForClaimOption
  ) extends ReasonForClaimAnswer

  object CompleteReasonForClaimAnswer {
    implicit val format: OFormat[CompleteReasonForClaimAnswer] = Json.format[CompleteReasonForClaimAnswer]
  }

  implicit class ReasonForClaimOps(
    private val a: ReasonForClaimAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteReasonForClaimAnswer => A,
      ifComplete: CompleteReasonForClaimAnswer => A
    ): A =
      a match {
        case i: IncompleteReasonForClaimAnswer => ifIncomplete(i)
        case c: CompleteReasonForClaimAnswer   => ifComplete(c)
      }
  }

  implicit val format: OFormat[ReasonForClaimAnswer] = derived.oformat()
}
