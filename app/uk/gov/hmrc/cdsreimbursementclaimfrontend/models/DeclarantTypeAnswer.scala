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

  final case class IncompleteDeclarationTypeAnswer(
    declarantType: Option[DeclarantType]
  ) extends DeclarantTypeAnswer

  object IncompleteDeclarationTypeAnswer {
    val empty: IncompleteDeclarationTypeAnswer = IncompleteDeclarationTypeAnswer(None)

    implicit val format: OFormat[IncompleteDeclarationTypeAnswer] = derived.oformat[IncompleteDeclarationTypeAnswer]()
  }

  final case class CompleteDeclarationTypeAnswer(
    declarantType: DeclarantType
  ) extends DeclarantTypeAnswer

  object CompleteMovementReferenceTypeAnswer {
    implicit val format: OFormat[CompleteDeclarationTypeAnswer] =
      derived.oformat[CompleteDeclarationTypeAnswer]()
  }

  implicit class DeclarantTypeAnswerOps(
    private val a: DeclarantTypeAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteDeclarationTypeAnswer => A,
      ifComplete: CompleteDeclarationTypeAnswer => A
    ): A =
      a match {
        case i: IncompleteDeclarationTypeAnswer => ifIncomplete(i)
        case c: CompleteDeclarationTypeAnswer   => ifComplete(c)
      }
  }

  implicit val format: OFormat[DeclarantTypeAnswer] = derived.oformat[DeclarantTypeAnswer]()
}
