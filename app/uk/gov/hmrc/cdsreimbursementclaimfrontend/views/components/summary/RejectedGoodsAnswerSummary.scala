package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import play.api.i18n.Messages
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList

trait RejectedGoodsAnswerSummary[A] {
  def render(key: String, answer: A)(implicit
                                     subKey: Option[String],
                                     messages: Messages
  ): SummaryList
}

