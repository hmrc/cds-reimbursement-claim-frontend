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

import play.api.libs.functional.syntax.toInvariantFunctorOps
import play.api.libs.json.Format

import java.util.function.Predicate
import play.api.i18n.Messages

final case class SortCode(value: String) extends AnyVal {

  def masked(implicit messages: Messages): String =
    messages("sort-code.mask", value.takeRight(2))
}

object SortCode {

  private val regex: Predicate[String] = "^\\d{6}$".r.pattern.asPredicate()

  def isValid(value: String): Boolean =
    regex.test(value)

  implicit val format: Format[SortCode] =
    implicitly[Format[String]].inmap(SortCode(_), _.value)
}
