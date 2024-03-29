/*
 * Copyright 2023 HM Revenue & Customs
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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat

sealed trait ReimbursementMethod

object ReimbursementMethod extends EnumerationFormat[ReimbursementMethod] {

  case object CurrentMonthAdjustment extends ReimbursementMethod
  case object BankAccountTransfer extends ReimbursementMethod
  case object Subsidy extends ReimbursementMethod

  override val values: Set[ReimbursementMethod] =
    Set(CurrentMonthAdjustment, BankAccountTransfer, Subsidy)

  val nonSubsidyValues: Set[ReimbursementMethod] =
    Set(CurrentMonthAdjustment, BankAccountTransfer)
}
