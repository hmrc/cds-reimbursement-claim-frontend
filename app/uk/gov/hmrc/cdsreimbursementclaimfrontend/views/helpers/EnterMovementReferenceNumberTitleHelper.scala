/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OrdinalNumber

object EnterMovementReferenceNumberTitleHelper {

  def single()(implicit messages: Messages): String =
    messages("enter-movement-reference-number.single.title")

  def multiple(pageOrdinalValue: Int, isSubsidy: Boolean)(implicit messages: Messages): String =
    messages.lang.language match {
      case "en" => multipleEnglish(pageOrdinalValue, isSubsidy)
      case "cy" => multipleWelsh(pageOrdinalValue, isSubsidy)
    }
  def scheduled()(implicit messages: Messages): String                                         =
    messages("enter-movement-reference-number.scheduled.title")

  private def multipleWelsh(pageOrdinalValue: Int, isSubsidy: Boolean)(implicit messages: Messages): String = {
    val maybeSubsidy = if (isSubsidy) ".subsidy" else ""
    pageOrdinalValue match {
      case x if x <= 20 =>
        messages(s"enter-movement-reference-number.multiple.title.$pageOrdinalValue$maybeSubsidy")
      case _            => messages(s"enter-movement-reference-number.multiple.title.default$maybeSubsidy")
    }
  }

  private def multipleEnglish(pageOrdinalValue: Int, isSubsidy: Boolean)(implicit messages: Messages): String = {
    val ordinalNumber = OrdinalNumber(pageOrdinalValue)
    val maybeSubsidy  = if (isSubsidy) ".subsidy" else ""
    messages(s"enter-movement-reference-number.multiple.title$maybeSubsidy", ordinalNumber)
  }
}
