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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimantDetailsAsImporterCompanyController.ClaimantDetailsAsImporterCompany

sealed trait ClaimantDetailsAsImporterCompanyAnswer extends Product with Serializable

object ClaimantDetailsAsImporterCompanyAnswer {

  final case class IncompleteClaimantDetailsAsImporterCompanyAnswer(
    claimantDetailsAsImporterCompany: Option[ClaimantDetailsAsImporterCompany]
  ) extends ClaimantDetailsAsImporterCompanyAnswer

  object IncompleteClaimantDetailsAsImporterCompanyAnswer {
    val empty: IncompleteClaimantDetailsAsImporterCompanyAnswer = IncompleteClaimantDetailsAsImporterCompanyAnswer(None)

    implicit val format: OFormat[IncompleteClaimantDetailsAsImporterCompanyAnswer] =
      derived.oformat[IncompleteClaimantDetailsAsImporterCompanyAnswer]()
  }

  final case class CompleteClaimantDetailsAsImporterCompanyAnswer(
    claimantDetailsAsImporterCompany: ClaimantDetailsAsImporterCompany
  ) extends ClaimantDetailsAsImporterCompanyAnswer

  object CompleteClaimantDetailsAsImporterCompanyAnswer {
    implicit val format: OFormat[CompleteClaimantDetailsAsImporterCompanyAnswer] =
      derived.oformat[CompleteClaimantDetailsAsImporterCompanyAnswer]()
  }

  implicit class ClaimantDetailsAsImporterCompanyAnswerOps(
    private val a: ClaimantDetailsAsImporterCompanyAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteClaimantDetailsAsImporterCompanyAnswer => A,
      ifComplete: CompleteClaimantDetailsAsImporterCompanyAnswer => A
    ): A =
      a match {
        case i: IncompleteClaimantDetailsAsImporterCompanyAnswer => ifIncomplete(i)
        case c: CompleteClaimantDetailsAsImporterCompanyAnswer   => ifComplete(c)
      }
  }

  implicit val format: OFormat[ClaimantDetailsAsImporterCompanyAnswer] =
    derived.oformat[ClaimantDetailsAsImporterCompanyAnswer]()
}
