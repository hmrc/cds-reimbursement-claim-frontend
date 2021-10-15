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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex

object OrdinalNumeralsHelper {

  val key = "ordinal."

  final def toMrnOrdinal(index: AssociatedMrnIndex)(implicit messages: Messages): String =
    index.toListIndex match {
      case 0 => messages(key + "lead")
      case i => toOrdinal(i + 1)
    }

  final def toOrdinal(integer: Int)(implicit messages: Messages): String =
    integer match {
      case 1           => messages(key + "first")
      case 2           => messages(key + "second")
      case 3           => messages(key + "third")
      case 4           => messages(key + "fourth")
      case 5           => messages(key + "fifth")
      case 6           => messages(key + "sixth")
      case 7           => messages(key + "seventh")
      case 8           => messages(key + "eighth")
      case 9           => messages(key + "ninth")
      case n if n < 20 => integer.toString() + messages(key + "th")
      case _           => integer.toString() + messages(key + suffix(integer % 10))
    }

  final def suffix(value: Int): String =
    value match {
      case 1 => "st"
      case 2 => "nd"
      case 3 => "rd"
      case 4 => "th"
      case 5 => "th"
      case 6 => "th"
      case 7 => "th"
      case 8 => "th"
      case 9 => "th"
    }

}
