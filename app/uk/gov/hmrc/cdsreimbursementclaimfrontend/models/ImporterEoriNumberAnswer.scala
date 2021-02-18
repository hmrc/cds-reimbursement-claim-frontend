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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterImporterEoriNumberController.ImporterEoriNumber

sealed trait ImporterEoriNumberAnswer extends Product with Serializable

object ImporterEoriNumberAnswer {

  final case class IncompleteImporterEoriNumberAnswer(
    importerEoriNumber: Option[ImporterEoriNumber]
  ) extends ImporterEoriNumberAnswer

  object IncompleteImporterEoriNumberAnswer {
    val empty: IncompleteImporterEoriNumberAnswer = IncompleteImporterEoriNumberAnswer(None)

    implicit val format: OFormat[IncompleteImporterEoriNumberAnswer] =
      derived.oformat[IncompleteImporterEoriNumberAnswer]()
  }

  final case class CompleteImporterEoriNumberAnswer(
    importerEoriNumber: ImporterEoriNumber
  ) extends ImporterEoriNumberAnswer

  object CompleteImporterEoriNumberAnswer {
    implicit val format: OFormat[CompleteImporterEoriNumberAnswer] =
      derived.oformat[CompleteImporterEoriNumberAnswer]()
  }

  implicit class ImporterEoriNumberAnswerOps(
    private val a: ImporterEoriNumberAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteImporterEoriNumberAnswer => A,
      ifComplete: CompleteImporterEoriNumberAnswer => A
    ): A =
      a match {
        case i: IncompleteImporterEoriNumberAnswer => ifIncomplete(i)
        case c: CompleteImporterEoriNumberAnswer   => ifComplete(c)
      }

    def importerEori: Option[ImporterEoriNumber] = a match {
      case IncompleteImporterEoriNumberAnswer(importerEoriNumber) => importerEoriNumber
      case CompleteImporterEoriNumberAnswer(importerEoriNumber)   => Some(importerEoriNumber)
    }

  }

  implicit val format: OFormat[ImporterEoriNumberAnswer] = derived.oformat[ImporterEoriNumberAnswer]()
}
