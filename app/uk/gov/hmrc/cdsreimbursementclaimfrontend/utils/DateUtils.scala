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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try

object DateUtils {

  def displayFormat(maybeDate: Option[String]): Option[String] =
    maybeDate.flatMap(displayFormat _)

  def displayFormat(date: String): Option[String] = {
    val result = for {
      t <- Try(LocalDate.parse(date, DateTimeFormatter.ofPattern("u-M-d")))
      f <- Try(DateTimeFormatter.ofPattern("d MMMM yyyy").format(t))
    } yield f
    result.toOption
  }
}
