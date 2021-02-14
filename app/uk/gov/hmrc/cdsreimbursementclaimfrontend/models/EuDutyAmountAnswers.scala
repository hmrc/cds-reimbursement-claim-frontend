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

sealed trait EuDutyAmountAnswers extends Product with Serializable

object EuDutyAmountAnswers {

  final case class IncompleteEuDutyAmountAnswer(
    euDutyAmounts: Option[EnterEuClaim]
  ) extends EuDutyAmountAnswers

  object IncompleteEuDutyAmountAnswer {
    val empty: IncompleteEuDutyAmountAnswer = IncompleteEuDutyAmountAnswer(None)

    implicit val format: OFormat[IncompleteEuDutyAmountAnswer] =
      derived.oformat[IncompleteEuDutyAmountAnswer]()
  }

  final case class CompleteEuDutyAmountAnswer(
    euDutyAmounts: EnterEuClaim
  ) extends EuDutyAmountAnswers

  object CompleteMovementReferenceTypeAnswer {
    implicit val format: OFormat[CompleteEuDutyAmountAnswer] =
      derived.oformat[CompleteEuDutyAmountAnswer]()
  }

  implicit class EuDutyAmountAnswersOps(
    private val a: EuDutyAmountAnswers
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteEuDutyAmountAnswer => A,
      ifComplete: CompleteEuDutyAmountAnswer => A
    ): A =
      a match {
        case i: IncompleteEuDutyAmountAnswer => ifIncomplete(i)
        case c: CompleteEuDutyAmountAnswer   => ifComplete(c)
      }

    def dutyAmounts: List[EnterDutyAmountsController.EuDutyAmount] = a match {
      case IncompleteEuDutyAmountAnswer(euDutyAmounts) =>
        euDutyAmounts match {
          case Some(value) => value.dutyAmounts
          case None        => List.empty
        }
      case CompleteEuDutyAmountAnswer(euDutyAmounts)   => euDutyAmounts.dutyAmounts
    }
  }

  implicit val format: OFormat[EuDutyAmountAnswers] =
    derived.oformat[EuDutyAmountAnswers]()
}
