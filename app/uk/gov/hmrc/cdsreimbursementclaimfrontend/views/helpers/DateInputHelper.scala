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

import cats.implicits.catsSyntaxEq
import play.api.data.FormError

object DateInputHelper {

  def withError(classs: String, error: Option[FormError], filter: String): String =
    error.fold(classs)(err =>
      if (
        err.messages.exists(message =>
          message
            .toLowerCase(java.util.Locale.ENGLISH)
            .contains(
              filter
            ) || message === "error.required" || message === "error.invalid" || (filter === "year" && message === "error.before1900")
        )
      )
        s"$classs govuk-input--error"
      else classs
    )
}
