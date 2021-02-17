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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.DeclarantType

sealed trait DeclarantTypeAnswer extends Product with Serializable

object DeclarantTypeAnswer {

  final case class IncompleteDeclarantTypeAnswer(
    declarantType: Option[DeclarantType]
  ) extends DeclarantTypeAnswer

  object IncompleteDeclarantTypeAnswer {
    val empty: IncompleteDeclarantTypeAnswer = IncompleteDeclarantTypeAnswer(None)

    implicit val format: OFormat[IncompleteDeclarantTypeAnswer] = derived.oformat[IncompleteDeclarantTypeAnswer]()
  }

  final case class CompleteDeclarantTypeAnswer(
    declarantType: DeclarantType
  ) extends DeclarantTypeAnswer

  object CompleteDeclarantTypeAnswer {
    implicit val format: OFormat[CompleteDeclarantTypeAnswer] =
      derived.oformat[CompleteDeclarantTypeAnswer]()
  }

  implicit class DeclarantTypeAnswerOps(
    private val a: DeclarantTypeAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteDeclarantTypeAnswer => A,
      ifComplete: CompleteDeclarantTypeAnswer => A
    ): A =
      a match {
        case i: IncompleteDeclarantTypeAnswer => ifIncomplete(i)
        case c: CompleteDeclarantTypeAnswer   => ifComplete(c)
      }
  }

  implicit val format: OFormat[DeclarantTypeAnswer] = derived.oformat[DeclarantTypeAnswer]()
}
