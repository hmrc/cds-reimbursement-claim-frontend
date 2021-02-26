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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.BankAccountController.BankAccountDetails

sealed trait BankAccountDetailsAnswer extends Product with Serializable

object BankAccountDetailsAnswer {

  final case class IncompleteBankAccountDetailAnswer(
    bankAccountDetails: Option[BankAccountDetails]
  ) extends BankAccountDetailsAnswer

  object IncompleteBankAccountDetailAnswer {
    val empty: IncompleteBankAccountDetailAnswer = IncompleteBankAccountDetailAnswer(None)

    implicit val format: OFormat[IncompleteBankAccountDetailAnswer] =
      derived.oformat[IncompleteBankAccountDetailAnswer]()
  }

  final case class CompleteBankAccountDetailAnswer(
    bankAccountDetails: BankAccountDetails
  ) extends BankAccountDetailsAnswer

  object CompleteBankAccountDetailAnswer {
    implicit val format: OFormat[CompleteBankAccountDetailAnswer] =
      derived.oformat[CompleteBankAccountDetailAnswer]()
  }

  implicit class BankAccountDetailsOps(
    private val a: BankAccountDetailsAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteBankAccountDetailAnswer => A,
      ifComplete: CompleteBankAccountDetailAnswer => A
    ): A =
      a match {
        case i: IncompleteBankAccountDetailAnswer => ifIncomplete(i)
        case c: CompleteBankAccountDetailAnswer   => ifComplete(c)
      }
  }

  implicit val format: OFormat[BankAccountDetailsAnswer] =
    derived.oformat[BankAccountDetailsAnswer]()
}
