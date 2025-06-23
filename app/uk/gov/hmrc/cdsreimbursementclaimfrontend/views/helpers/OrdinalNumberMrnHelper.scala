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

import cats.syntax.eq.*
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OrdinalNumber

object OrdinalNumberMrnHelper {

  def apply(number: Int, isFirstOnPage: Boolean = false)(implicit messages: Messages): String =
    messages.lang.language match {
      case "en" => applyEnglish(number, isFirstOnPage)
      case "cy" => applyWelsh(number, isFirstOnPage)
    }

  private def applyWelsh(number: Int, isFirstOnPage: Boolean)(implicit messages: Messages): String =
    if number <= 20 then {
      if isFirstOnPage && number === 1 then messages(s"ordinal-number-mrn-first-on-page.numeric")
      else messages(s"ordinal-number-mrn.numeric.$number")
    } else {
      messages(s"ordinal-number-mrn.default")
    }

  private def applyEnglish(number: Int, isFirstOnPage: Boolean)(implicit messages: Messages): String =
    if isFirstOnPage && number === 1 then messages("ordinal-number-mrn-first-on-page")
    else {
      val ordinalNumber = OrdinalNumber.numeric(number)
      messages("ordinal-number-mrn", ordinalNumber.capitalize)
    }
}
