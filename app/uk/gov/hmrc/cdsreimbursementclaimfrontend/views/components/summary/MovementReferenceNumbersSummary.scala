package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList

class MovementReferenceNumbersSummary extends AnswerSummary[Seq[MRN]] {

  def render(key: String, answers: Seq[MRN])(implicit router: ReimbursementRoutes, messages: Messages): SummaryList =
    ???
}
