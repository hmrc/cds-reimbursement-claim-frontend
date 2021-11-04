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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList

package object summary {

  implicit val claimTypeSummary: ClaimTypeSummary                                 = new ClaimTypeSummary
  implicit val basisOfClaimSummary: BasisOfClaimSummary                           = new BasisOfClaimSummary
  implicit val claimsAnswerSummary: ClaimsAnswerSummary                           = new ClaimsAnswerSummary
  implicit val multipleClaimsAnswerSummary: MultipleClaimsAnswerSummary           = new MultipleClaimsAnswerSummary
  implicit val mrnSummary: MovementReferenceNumberSummary                         = new MovementReferenceNumberSummary
  implicit val mrnsSummary: MovementReferenceNumbersSummary                       = new MovementReferenceNumbersSummary
  implicit val commodityDetailsSummary: CommodityDetailsSummary                   = new CommodityDetailsSummary
  implicit val displayDeclarationSummary: DisplayDeclarationSummary               = new DisplayDeclarationSummary
  implicit val reimbursementMethodAnswerSummary: ReimbursementMethodAnswerSummary = new ReimbursementMethodAnswerSummary
  implicit val reimbursementsSummary: DutyAndTaxCodeReimbursementSummary          = new DutyAndTaxCodeReimbursementSummary
  implicit val taxCodeReimbursementSummary: TaxCodeReimbursementSummary           = new TaxCodeReimbursementSummary
  implicit val cdsClaimantDetailsSummary: CdsClaimantDetailsSummary               = new CdsClaimantDetailsSummary
  implicit val supportingEvidenceSummary: SupportingEvidenceSummary               = new SupportingEvidenceSummary
  implicit val scheduledDocumentSummary: ScheduledDocumentSummary                 = new ScheduledDocumentSummary
  implicit val contactDetailsSummary: ContactDetailsSummary                       = new ContactDetailsSummary

  implicit class AnswerSummaryOps[A](private val answer: A) extends AnyVal {

    def summary(key: String, router: ReimbursementRoutes)(implicit
      answerSummary: AnswerSummary[A],
      messages: Messages
    ): SummaryList =
      answerSummary.render(key, answer)(router, messages)

    def summary(
      key: String
    )(implicit answerSummary: AnswerSummary[A], router: ReimbursementRoutes, messages: Messages): SummaryList =
      answerSummary.render(key, answer)
  }

  implicit class SummaryListOps(private val summaryList: SummaryList) extends AnyVal {
    def drop(n: Int): SummaryList =
      SummaryList(summaryList.rows.drop(n))
  }
}
