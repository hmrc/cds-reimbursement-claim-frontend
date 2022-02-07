/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.syntax.eq._
import play.api.i18n.Messages

/** This is an I18N-aware replacement for [[OrdinalNumeral]] */
object OrdinalNumber {

  def label(index: Int)(implicit messages: Messages): String = {
    val i     = Math.abs(index)
    val key   = s"ordinal.label.$i"
    val label = messages(key)
    if (label === key) { // if there is no ordinal for number
      val key    = s"ordinal.suffix.${if (i > 9 && i < 20) "1x" else i % 10}"
      val suffix = messages(key)
      if (suffix === key) // if there is no suffix for number
        s"$i"
      else
        s"$i$suffix"
    } else label
  }
}
