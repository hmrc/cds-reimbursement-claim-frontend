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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.JourneyBindable
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList

package object summary {

  implicit val commodityDetailsSummary: CommodityDetailsSummary     = new CommodityDetailsSummary
  implicit val claimTypeSummary: ClaimTypeSummary                   = new ClaimTypeSummary
  implicit val entryNumberSummary: EntryNumberSummary               = new EntryNumberSummary
  implicit val mrnSummary: MrnSummary                               = new MrnSummary
  implicit val basisOfClaimSummary: BasisOfClaimSummary             = new BasisOfClaimSummary
  implicit val supportingEvidenceSummary: SupportingEvidenceSummary = new SupportingEvidenceSummary

  implicit class AnswerSummaryOps[A](val answer: A) extends AnyVal {
    def review(
      key: String
    )(implicit answerSummary: AnswerSummary[A], journey: JourneyBindable, messages: Messages): SummaryList =
      answerSummary.render(key, answer)
  }
}
