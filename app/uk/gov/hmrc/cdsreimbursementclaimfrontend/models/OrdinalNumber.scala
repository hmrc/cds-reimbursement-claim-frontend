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

import play.api.i18n.Messages

/** This is an I18N-aware replacement for [[OrdinalNumeral]] */
object OrdinalNumber {

  def label(index: Int)(implicit messages: Messages): String = {
    val i   = Math.abs(index)
    val key = s"ordinal.label.$i"
    messages
      .translate(key, Seq.empty)
      .fold { // if there is no ordinal for number
        val key = s"ordinal.suffix.${if (i > 9 && i < 20) "1x" else i % 10}"
        messages.translate(key, Seq.empty).fold(s"$i")(suffix => s"$i$suffix")
      }(identity)
  }
}
