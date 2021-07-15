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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarationDetailsController.EntryDeclarationDetails

sealed trait DeclarationDetailsAnswer extends Product with Serializable

object DeclarationDetailsAnswer {

  final case class IncompleteDeclarationDetailsAnswer(
    declarationDetails: Option[EntryDeclarationDetails]
  ) extends DeclarationDetailsAnswer

  object IncompleteDeclarationDetailsAnswer {
    val empty: IncompleteDeclarationDetailsAnswer = IncompleteDeclarationDetailsAnswer(None)

    implicit val format: OFormat[IncompleteDeclarationDetailsAnswer] =
      derived.oformat[IncompleteDeclarationDetailsAnswer]()
  }

  final case class CompleteDeclarationDetailsAnswer(
    declarationDetails: EntryDeclarationDetails
  ) extends DeclarationDetailsAnswer

  object CompleteDeclarationDetailsAnswer {
    implicit val format: OFormat[CompleteDeclarationDetailsAnswer] =
      derived.oformat[CompleteDeclarationDetailsAnswer]()
  }

  implicit class DeclarantDetailAnswersOps(
    private val a: DeclarationDetailsAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteDeclarationDetailsAnswer => A,
      ifComplete: CompleteDeclarationDetailsAnswer => A
    ): A =
      a match {
        case i: IncompleteDeclarationDetailsAnswer => ifIncomplete(i)
        case c: CompleteDeclarationDetailsAnswer   => ifComplete(c)
      }

    def declarationDetail: Option[EntryDeclarationDetails] = a match {
      case IncompleteDeclarationDetailsAnswer(declarationDetails) => declarationDetails
      case CompleteDeclarationDetailsAnswer(declarationDetails)   => Some(declarationDetails)
    }
  }

  implicit val format: OFormat[DeclarationDetailsAnswer] = derived.oformat[DeclarationDetailsAnswer]()
}
