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

sealed trait DeclarationDetailAnswers extends Product with Serializable

object DeclarationDetailAnswers {

  final case class IncompleteDeclarationDetailAnswer(
    declarationDetails: Option[EntryDeclarationDetails]
  ) extends DeclarationDetailAnswers

  object IncompleteDeclarationDetailAnswer {
    val empty: IncompleteDeclarationDetailAnswer = IncompleteDeclarationDetailAnswer(None)

    implicit val format: OFormat[IncompleteDeclarationDetailAnswer] =
      derived.oformat[IncompleteDeclarationDetailAnswer]()
  }

  final case class CompleteDeclarationDetailAnswer(
    declarationDetails: EntryDeclarationDetails
  ) extends DeclarationDetailAnswers

  object CompleteDeclarationDetailAnswer {
    implicit val format: OFormat[CompleteDeclarationDetailAnswer] =
      derived.oformat[CompleteDeclarationDetailAnswer]()
  }

  implicit class DeclarantDetailAnswersOps(
    private val a: DeclarationDetailAnswers
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteDeclarationDetailAnswer => A,
      ifComplete: CompleteDeclarationDetailAnswer => A
    ): A =
      a match {
        case i: IncompleteDeclarationDetailAnswer => ifIncomplete(i)
        case c: CompleteDeclarationDetailAnswer   => ifComplete(c)
      }

    def declarationDetail: Option[EntryDeclarationDetails] = a match {
      case IncompleteDeclarationDetailAnswer(declarationDetails) => declarationDetails
      case CompleteDeclarationDetailAnswer(declarationDetails)   => Some(declarationDetails)
    }
  }

  implicit val format: OFormat[DeclarationDetailAnswers] = derived.oformat[DeclarationDetailAnswers]()
}
