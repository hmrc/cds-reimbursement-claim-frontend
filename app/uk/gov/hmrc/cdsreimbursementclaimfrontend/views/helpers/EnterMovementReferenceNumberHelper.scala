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

object EnterMovementReferenceNumberHelper {

  def titleSingle()(implicit messages: Messages): String =
    messages("enter-movement-reference-number.single.title")

  def titleMultiple(pageNumber: Int)(implicit messages: Messages): String =
    messages.lang.language match {
      case "en" => titleMultipleEnglish(pageNumber)
      case "cy" => titleMultipleWelsh(pageNumber)
    }

  def titleScheduled()(implicit messages: Messages): String =
    messages("enter-movement-reference-number.scheduled.title")

  private def titleMultipleWelsh(pageNumber: Int)(implicit messages: Messages): String =
    pageNumber match {
      case x if x <= 20 =>
        messages(s"enter-movement-reference-number.multiple.title.$pageNumber")
      case _            => messages(s"enter-movement-reference-number.multiple.title.default")
    }

  private def titleMultipleEnglish(pageNumber: Int)(implicit messages: Messages): String = {
    val ordinalNumber      = OrdinalNumber(pageNumber)
    val firstCharUppercase =
      messages(s"enter-movement-reference-number.multiple.title", ordinalNumber).charAt(0).toUpper
    messages(s"enter-movement-reference-number.multiple.title", ordinalNumber)
      .replaceFirst("^.", firstCharUppercase.toString)
  }

  def labelSingle()(implicit messages: Messages): String =
    messages("enter-movement-reference-number.single.label")

  def labelMultiple(pageNumber: Int)(implicit messages: Messages): String =
    messages.lang.language match {
      case "en" => labelMultipleEnglish(pageNumber)
      case "cy" => labelMultipleWelsh(pageNumber)
    }

  def labelScheduled()(implicit messages: Messages): String =
    messages("enter-movement-reference-number.scheduled.label")

  private def labelMultipleWelsh(pageNumber: Int)(implicit messages: Messages): String =
    pageNumber match {
      case x if x <= 20 =>
        messages(s"enter-movement-reference-number.multiple.label.$pageNumber")
      case _            => messages(s"enter-movement-reference-number.multiple.label.default")
    }

  private def labelMultipleEnglish(pageNumber: Int)(implicit messages: Messages): String = {
    val ordinalNumber = OrdinalNumber(pageNumber)
    messages(s"enter-movement-reference-number.multiple.label", ordinalNumber)
  }
}
