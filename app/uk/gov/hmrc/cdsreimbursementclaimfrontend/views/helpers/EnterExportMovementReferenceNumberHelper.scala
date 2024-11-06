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

object EnterExportMovementReferenceNumberHelper {

  def title(pageNumber: Int)(implicit messages: Messages): String =
    messages.lang.language match {
      case "en" => titleEnglish(pageNumber)
      case "cy" => titleWelsh(pageNumber)
    }

  private def titleWelsh(pageNumber: Int)(implicit messages: Messages): String =
    pageNumber match {
      case x if x <= 20 =>
        messages(s"enter-export-movement-reference-number.next.securities.title.$pageNumber")
      case _            => messages(s"enter-export-movement-reference-number.next.securities.title.default")
    }

  private def titleEnglish(pageNumber: Int)(implicit messages: Messages): String = {
    val ordinalNumber = OrdinalNumber(pageNumber)
    messages("enter-export-movement-reference-number.next.securities.title", ordinalNumber)
  }

  def label(pageNumber: Int)(implicit messages: Messages): String =
    messages.lang.language match {
      case "en" => labelEnglish(pageNumber)
      case "cy" => labelWelsh(pageNumber)
    }

  private def labelWelsh(pageNumber: Int)(implicit messages: Messages): String =
    pageNumber match {
      case x if x <= 20 =>
        messages(s"enter-export-movement-reference-number.next.securities.label.$pageNumber")
      case _            => messages(s"enter-export-movement-reference-number.next.securities.label.default")
    }

  private def labelEnglish(pageNumber: Int)(implicit messages: Messages): String = {
    val ordinalNumber = OrdinalNumber(pageNumber)

    messages("enter-export-movement-reference-number.next.securities.label", ordinalNumber)
  }
}
