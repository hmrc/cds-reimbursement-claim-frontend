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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectNumberOfClaimsController.SelectNumberOfClaimsType

//case class SelectNumberOfClaimsAnswer(selectNumberOfClaimsChoice: Option[SelectNumberOfClaimsType])
//
//object SelectNumberOfClaimsAnswer {
//  implicit val selectNumberOfClaimsAnswerFormat:OFormat[SelectNumberOfClaimsType] = Json.format[SelectNumberOfClaimsType]
//}

sealed trait SelectNumberOfClaimsAnswer extends Product with Serializable

object SelectNumberOfClaimsAnswer {

  final case class IncompleteSelectNumberOfClaimsAnswer(
    selectNumberOfClaimsChoice: Option[SelectNumberOfClaimsType]
  ) extends SelectNumberOfClaimsAnswer

  object IncompleteSelectNumberOfClaimsAnswer {
    val empty: IncompleteSelectNumberOfClaimsAnswer = IncompleteSelectNumberOfClaimsAnswer(None)

    implicit val format: OFormat[IncompleteSelectNumberOfClaimsAnswer] =
      derived.oformat[IncompleteSelectNumberOfClaimsAnswer]()
  }

  final case class CompleteSelectNumberOfClaimsAnswer(
    selectNumberOfClaimsChoice: SelectNumberOfClaimsType
  ) extends SelectNumberOfClaimsAnswer

  object CompleteSelectNumberOfClaimsAnswer {
    implicit val format: OFormat[CompleteSelectNumberOfClaimsAnswer] =
      derived.oformat[CompleteSelectNumberOfClaimsAnswer]()
  }

  implicit class ClaimantDetailsAsIndividualAnswerOps(
    private val a: SelectNumberOfClaimsAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteSelectNumberOfClaimsAnswer => A,
      ifComplete: CompleteSelectNumberOfClaimsAnswer => A
    ): A =
      a match {
        case i: IncompleteSelectNumberOfClaimsAnswer => ifIncomplete(i)
        case c: CompleteSelectNumberOfClaimsAnswer   => ifComplete(c)
      }
  }

  implicit val format: OFormat[SelectNumberOfClaimsAnswer] = derived.oformat[SelectNumberOfClaimsAnswer]()
}
