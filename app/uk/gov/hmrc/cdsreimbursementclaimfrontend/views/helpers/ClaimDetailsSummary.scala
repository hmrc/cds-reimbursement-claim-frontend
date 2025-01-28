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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.routes as multipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.routes as scheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.routes as singleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.NewEoriAndDan
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

object ClaimDetailsSummary {

  def apply(
    basisOfClaim: BasisOfOverpaymentClaim,
    additionalDetails: String,
    basisOfClaimChangeCallOpt: Option[Call],
    additionalDetailsChangeCallOpt: Option[Call],
    newEoriAndDanOpt: Option[NewEoriAndDan],
    journeyType: String,
    duplicateMovementReferenceNumber: Option[MRN] = None
  )(implicit
    messages: Messages
  ): SummaryList = {
    val isPrintView = additionalDetailsChangeCallOpt.isEmpty && basisOfClaimChangeCallOpt.isEmpty
    SummaryList(
      Seq(
        Some(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.basis-of-claim"))),
            value = Value(
              HtmlContent(messages(s"select-basis-for-claim.reason.$basisOfClaim"))
            ),
            actions = basisOfClaimChangeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages("check-your-answers.basis-of-claim"))
                  )
                )
              )
            )
          )
        ),
        duplicateMovementReferenceNumber.map { duplicateMrn =>
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.duplicate-mrn"))),
            value = Value(
              Text(duplicateMrn.value)
            ),
            actions =
              if isPrintView then None
              else
                Some(
                  Actions(
                    items = Seq(
                      ActionItem(
                        href = singleRoutes.EnterDuplicateMovementReferenceNumberController.show.url,
                        content = Text(messages("cya.change")),
                        visuallyHiddenText = Some(messages("check-your-answers.duplicate-mrn"))
                      )
                    )
                  )
                )
          )
        },
        newEoriAndDanOpt.map { newEoriAndDan =>
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.new-eori"))),
            value = Value(
              Text(newEoriAndDan.eori.value)
            ),
            actions =
              if isPrintView then None
              else
                Some(
                  Actions(
                    items = Seq(
                      ActionItem(
                        href = getNewEoriPageUrl(journeyType),
                        content = Text(messages("cya.change")),
                        visuallyHiddenText = Some(messages("check-your-answers.new-eori"))
                      )
                    )
                  )
                )
          )
        },
        newEoriAndDanOpt.map { newEoriAndDan =>
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.new-dan"))),
            value = Value(
              Text(newEoriAndDan.dan)
            ),
            actions =
              if isPrintView then None
              else
                Some(
                  Actions(
                    items = Seq(
                      ActionItem(
                        href = getNewDanPageUrl(journeyType),
                        content = Text(messages("cya.change")),
                        visuallyHiddenText = Some(messages("check-your-answers.new-dan"))
                      )
                    )
                  )
                )
          )
        },
        Some(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.additional-info"))),
            value = Value(
              Text(additionalDetails)
            ),
            actions = additionalDetailsChangeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages("check-your-answers.additional-info"))
                  )
                )
              )
            )
          )
        )
      ).flatten
    )
  }

  private def getNewEoriPageUrl(journey: String): String = journey match {
    case "single"    => singleRoutes.EnterNewEoriNumberController.show.url
    case "multiple"  => multipleRoutes.EnterNewEoriNumberController.show.url
    case "scheduled" => scheduledRoutes.EnterNewEoriNumberController.show.url
  }

  private def getNewDanPageUrl(journey: String): String = journey match {
    case "single"    => singleRoutes.EnterNewDanController.show.url
    case "multiple"  => multipleRoutes.EnterNewDanController.show.url
    case "scheduled" => scheduledRoutes.EnterNewDanController.show.url
  }

  def apply(
    basisOfClaim: BasisOfRejectedGoodsClaim,
    basisOfClaimSpecialCircumstances: Option[String],
    detailsOfRejectedGoods: String,
    methodOfDisposal: MethodOfDisposal,
    basisOfClaimChangeCallOpt: Option[Call],
    basisOfClaimSpecialCircumstancesChangeCallOpt: Option[Call],
    detailsOfRejectedGoodsChangeCallOpt: Option[Call],
    methodOfDisposalChangeCallOpt: Option[Call]
  )(implicit messages: Messages): SummaryList =
    SummaryList(
      Seq(
        Some(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.basis-of-claim"))),
            value = Value(
              HtmlContent(messages(s"select-basis-for-claim.rejected-goods.reason.$basisOfClaim"))
            ),
            actions = basisOfClaimChangeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages("check-your-answers.basis-of-claim"))
                  )
                )
              )
            )
          )
        ),
        basisOfClaimSpecialCircumstances.map { sc =>
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.special-circumstances"))),
            value = Value(Text(sc)),
            actions = basisOfClaimSpecialCircumstancesChangeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages("check-your-answers.special-circumstances"))
                  )
                )
              )
            )
          )
        },
        Some(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.additional-info"))),
            value = Value(
              Text(detailsOfRejectedGoods)
            ),
            actions = detailsOfRejectedGoodsChangeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages("check-your-answers.additional-info"))
                  )
                )
              )
            )
          )
        ),
        Some(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.disposal-method.label"))),
            value = Value(Text(messages(s"select-method-of-disposal.rejected-goods.method.$methodOfDisposal"))),
            actions = methodOfDisposalChangeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages("check-your-answers.disposal-method.label"))
                  )
                )
              )
            )
          )
        )
      ).flatten
    )
}
