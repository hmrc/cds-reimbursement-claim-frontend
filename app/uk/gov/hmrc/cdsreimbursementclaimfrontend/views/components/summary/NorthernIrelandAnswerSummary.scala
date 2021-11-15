package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.govukfrontend.views.Aliases.{ActionItem, Key, Text}
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{Actions, SummaryList, SummaryListRow, Value}

class NorthernIrelandAnswerSummary extends AnswerSummary[YesNo] {

  def render(key: String, answer: YesNo)(implicit router: ReimbursementRoutes, messages: Messages): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(Text(messages(s"$key.label"))),
          value = Value(Text(answer.toString)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.ClaimNorthernIrelandController.changeNorthernIrelandClaim(router.journeyBindable).url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.label"))
                )
              )
            )
          )
        )
      )
    )
}
