package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList

class NorthernIrelandAnswerSummary extends AnswerSummary[YesNo] {

  def render(key: String, answer: YesNo)(implicit router: ReimbursementRoutes, messages: Messages): SummaryList =
    SummaryList(

    )


  /*

    def makeNorthernIrelandClaimSummary(
    completeClaim: CompleteClaim
  )(implicit messages: Messages, journey: JourneyBindable): List[SummaryListRow] =
    completeClaim.northernIrelandAnswer.fold(List.empty[SummaryListRow])(claimNorthernIrelandAnswer =>
      List(
        SummaryListRow(
          key = Key(Text(messages(s"$key.northern-ireland-claim.label"))),
          value = Value(Text(claimNorthernIrelandAnswer.toString)),
          actions = Some(
            Actions(
              items = Seq(
                ActionItem(
                  href = s"${routes.ClaimNorthernIrelandController.changeNorthernIrelandClaim(journey).url}",
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages(s"$key.northern-ireland-claim.label"))
                )
              )
            )
          )
        )
      )
    )
   */
}
