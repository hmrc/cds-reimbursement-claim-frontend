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
import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

import java.util.function.Predicate

final case class AccountNumber(value: String) extends AnyVal {

  def masked(implicit messages: Messages): String =
    messages("account-number.mask", value.takeRight(4))
}

object AccountNumber {

  private val regex: Predicate[String] = "^\\d+$".r.pattern.asPredicate()

  def hasValidLength(value: String): Boolean =
    value.length >= 6 && value.length <= 8

  def isValid(value: String): Boolean =
    regex.test(value)

  implicit val accountNumberFormat: Format[AccountNumber] =
    SimpleStringFormat(AccountNumber(_), _.value)
}
