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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDutyAmountsController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDutyAmountsController.EnterEuClaim

sealed trait EUDutyAmountAnswers extends Product with Serializable

object EUDutyAmountAnswers {

  final case class IncompleteEUDutyAmountAnswer(
    euDutyAmounts: Option[EnterEuClaim]
  ) extends EUDutyAmountAnswers

  object IncompleteEUDutyAmountAnswer {
    val empty: IncompleteEUDutyAmountAnswer = IncompleteEUDutyAmountAnswer(None)

    implicit val format: OFormat[IncompleteEUDutyAmountAnswer] =
      derived.oformat[IncompleteEUDutyAmountAnswer]()
  }

  final case class CompleteEUDutyAmountAnswer(
    euDutyAmounts: EnterEuClaim
  ) extends EUDutyAmountAnswers

  object CompleteEUDutyAmountAnswer {
    implicit val format: OFormat[CompleteEUDutyAmountAnswer] =
      derived.oformat[CompleteEUDutyAmountAnswer]()
  }

  implicit class EuDutyAmountAnswersOps(
    private val a: EUDutyAmountAnswers
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteEUDutyAmountAnswer => A,
      ifComplete: CompleteEUDutyAmountAnswer => A
    ): A =
      a match {
        case i: IncompleteEUDutyAmountAnswer => ifIncomplete(i)
        case c: CompleteEUDutyAmountAnswer   => ifComplete(c)
      }

    def dutyAmounts: List[EnterDutyAmountsController.EuDutyAmount] = a match {
      case IncompleteEUDutyAmountAnswer(euDutyAmounts) =>
        euDutyAmounts match {
          case Some(value) => value.dutyAmounts
          case None        => List.empty
        }
      case CompleteEUDutyAmountAnswer(euDutyAmounts)   => euDutyAmounts.dutyAmounts
    }
    def maybeEuDuty: Option[EnterEuClaim]                          = a match {
      case IncompleteEUDutyAmountAnswer(euDutyAmounts) => euDutyAmounts
      case CompleteEUDutyAmountAnswer(euDutyAmounts)   => Some(euDutyAmounts)
    }
  }

  implicit val format: OFormat[EUDutyAmountAnswers] =
    derived.oformat[EUDutyAmountAnswers]()
}
