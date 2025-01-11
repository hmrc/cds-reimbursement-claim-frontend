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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers

import play.api.i18n.Messages
import cats.syntax.eq._
import java.time.LocalDate
import java.util.Locale

object DateFormatter {

  def toDisplayDate(dateString: String)(implicit messages: Messages): String = {
    val split = dateString.trim.split(" ")
    if split.length === 3 then s"${split(0)} ${messages(s"month.${split(1).toLowerCase(Locale.ENGLISH)}")} ${split(2)}"
    else dateString
  }

  def parseYyyyMmDdToDisplayDate(dateString: String)(implicit messages: Messages): String = {
    val date = LocalDate.parse(dateString)
    toDisplayDate(date)
  }

  def toDisplayDate(date: LocalDate)(implicit messages: Messages): String =
    s"${date.getDayOfMonth} ${messages(s"month.${date.getMonth.name().toLowerCase(Locale.ENGLISH)}")} ${date.getYear}"

}
