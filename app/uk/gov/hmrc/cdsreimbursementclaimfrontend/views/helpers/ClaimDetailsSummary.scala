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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.NewEoriAndDan
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

object ClaimDetailsSummary {

  def apply(
    basisOfClaim: BasisOfOverpaymentClaim,
    additionalDetails: String,
    basisOfClaimChangeCallOpt: Option[Call],
    additionalDetailsChangeCallOpt: Option[Call],
    newEoriAndDanOpt: Option[NewEoriAndDan],
    duplicateMovementReferenceNumber: Option[MRN] = None
  )(implicit
    messages: Messages
  ): SummaryList = {
    val isPdf = additionalDetailsChangeCallOpt.isEmpty && basisOfClaimChangeCallOpt.isEmpty
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
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = routes.EnterDuplicateMovementReferenceNumberController.show.url,
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
              if (isPdf) None
              else
                Some(
                  Actions(
                    items = Seq(
                      ActionItem(
                        href = routes.EnterNewEoriNumberController.show.url,
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
              if (isPdf) None
              else
                Some(
                  Actions(
                    items = Seq(
                      ActionItem(
                        href = routes.EnterNewDanController.show.url,
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
}
