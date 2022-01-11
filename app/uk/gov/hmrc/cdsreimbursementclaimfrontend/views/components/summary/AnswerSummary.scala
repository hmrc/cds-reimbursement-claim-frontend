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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList

trait AnswerSummary[A] {

  def render(answer: A, key: String, subKey: Option[String], changeCallOpt: Option[Call])(implicit
    messages: Messages
  ): SummaryList

  final def apply(answer: A, key: String, subKey: Option[String], changeCall: Call)(implicit
    messages: Messages
  ): SummaryList =
    render(answer, key, subKey, Some(changeCall))

  final def apply(answer: A, key: String, changeCall: Call)(implicit
    messages: Messages
  ): SummaryList =
    render(answer, key, None, Some(changeCall))

  final def apply(answer: A, key: String, subKey: Option[String])(implicit
    messages: Messages
  ): SummaryList =
    render(answer, key, subKey, None)

  final def apply(answer: A, key: String)(implicit
    messages: Messages
  ): SummaryList =
    render(answer, key, None, None)
}
