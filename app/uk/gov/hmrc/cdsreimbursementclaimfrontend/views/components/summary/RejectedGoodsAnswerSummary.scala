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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList

trait RejectedGoodsAnswerSummary[A] {
  def render(key: String, answer: A)(implicit
    subKey: Option[String],
    messages: Messages
  ): SummaryList
}

object RejectedGoodsAnswerSummary {

  def apply[A](key: String, subKey: Option[String], value: A)(implicit
    answerSummary: RejectedGoodsAnswerSummary[A],
    messages: Messages
  ): SummaryList = answerSummary.render(key, value)(subKey, messages)

  def apply[A](key: String, value: A)(implicit
    answerSummary: RejectedGoodsAnswerSummary[A],
    messages: Messages
  ): SummaryList = apply(key, None, value)

  implicit val claimantDetailsSummary: RejectedGoodsAnswerSummary[(MrnContactDetails, ContactAddress, Call, Call)] =
    ClaimantDetailsSummary

  implicit val declarationDetailsSummary: RejectedGoodsAnswerSummary[DisplayDeclaration] =
    DisplayDeclarationSummary

}
