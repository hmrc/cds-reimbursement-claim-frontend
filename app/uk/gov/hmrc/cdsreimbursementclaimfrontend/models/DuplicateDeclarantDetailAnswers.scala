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

sealed trait DuplicateDeclarantDetailAnswers extends Product with Serializable

object DuplicateDeclarantDetailAnswers {

  final case class IncompleteDuplicateDeclarationDetailAnswer(
    duplicateDeclaration: Option[EntryDeclarationDetails]
  ) extends DuplicateDeclarantDetailAnswers

  object IncompleteDuplicateDeclarationDetailAnswer {
    val empty: IncompleteDuplicateDeclarationDetailAnswer = IncompleteDuplicateDeclarationDetailAnswer(None)

    implicit val format: OFormat[IncompleteDuplicateDeclarationDetailAnswer] =
      derived.oformat[IncompleteDuplicateDeclarationDetailAnswer]()
  }

  final case class CompleteDuplicateDeclarationDetailAnswer(
    duplicateDeclaration: Option[EntryDeclarationDetails]
  ) extends DuplicateDeclarantDetailAnswers

  object CompleteDuplicateDeclarationDetailAnswer {
    implicit val format: OFormat[CompleteDuplicateDeclarationDetailAnswer] =
      derived.oformat[CompleteDuplicateDeclarationDetailAnswer]()
  }

  implicit class DuplicateDeclarantDetailAnswersOps(
    private val a: DuplicateDeclarantDetailAnswers
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteDuplicateDeclarationDetailAnswer => A,
      ifComplete: CompleteDuplicateDeclarationDetailAnswer => A
    ): A =
      a match {
        case i: IncompleteDuplicateDeclarationDetailAnswer => ifIncomplete(i)
        case c: CompleteDuplicateDeclarationDetailAnswer   => ifComplete(c)
      }

    def duplicateDeclaration: Option[EntryDeclarationDetails] = a match {
      case IncompleteDuplicateDeclarationDetailAnswer(duplicateDeclaration) => duplicateDeclaration
      case CompleteDuplicateDeclarationDetailAnswer(duplicateDeclaration)   => duplicateDeclaration
    }
  }

  implicit val format: OFormat[DuplicateDeclarantDetailAnswers] = derived.oformat[DuplicateDeclarantDetailAnswers]()
}
