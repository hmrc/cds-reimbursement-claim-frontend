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

object OrdinalNumberExportMrnHelper {

  def apply(number: Int)(implicit messages: Messages): String =
    messages.lang.language match {
      case "en" => applyEnglish(number)
      case "cy" => applyWelsh(number)
    }

  private def applyWelsh(number: Int)(implicit messages: Messages): String =
    if number <= 20 then {
      messages(s"ordinal-number-mrn.export.$number")
    } else {
      messages(s"ordinal-number-mrn.export.default")
    }

  private def applyEnglish(number: Int)(implicit messages: Messages): String = {
    val ordinalNumber = OrdinalNumber(number)
    messages("ordinal-number-mrn.export", ordinalNumber.capitalize)
  }
}
