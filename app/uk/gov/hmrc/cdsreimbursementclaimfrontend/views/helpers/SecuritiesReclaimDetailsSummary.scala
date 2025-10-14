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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.ActionItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow

import java.util.Locale
import scala.collection.immutable.SortedMap

object SecuritiesReclaimDetailsSummary {

  def apply(
    securityDepositId: String,
    reclaims: SortedMap[TaxCode, BigDecimal],
    declaration: ImportDeclaration,
    key: String,
    fullAmountChangeCallOpt: Option[String => Call],
    dutiesSelectionChangeCallOpt: Option[String => Call],
    reclaimAmountChangeCallOpt: Option[(String, TaxCode) => Call]
  )(implicit
    messages: Messages
  ): SummaryList = SummaryList(
    Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.claim-full-amount.label"))),
        value = Value(
          Text(
            messages(
              if declaration.isFullSecurityAmount(securityDepositId, reclaims.values.sum) then
                s"$key.claim-full-amount.yes"
              else s"$key.claim-full-amount.no"
            )
          )
        ),
        actions = fullAmountChangeCallOpt.map(fullAmountChangeCall =>
          Actions(
            items = Seq(
              ActionItem(
                href = fullAmountChangeCall(securityDepositId).url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  messages(
                    "check-claim.securities.hidden.claim-full-amount",
                    messages(s"$key.claim-full-amount.label").toLowerCase(Locale.ROOT),
                    securityDepositId
                  )
                )
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.duties-selected.label"))),
        value = Value(
          HtmlContent(
            reclaims.keys.toList.sorted
              .map(taxCode => messages(s"tax-code.$taxCode"))
              .mkString("<br>")
          )
        ),
        actions = dutiesSelectionChangeCallOpt.map(dutiesSelectionChangeCall =>
          Actions(
            items = Seq(
              ActionItem(
                href = dutiesSelectionChangeCall(securityDepositId).url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  messages(
                    "check-claim.securities.hidden.duties-selected",
                    messages(s"$key.duties-selected.label").toLowerCase(Locale.ROOT),
                    securityDepositId
                  )
                )
              )
            )
          )
        )
      )
    ) ++ reclaims.toList.sorted.zipWithIndex.map { case ((taxCode, amount), _) =>
      SummaryListRow(
        key = Key(HtmlContent(messages(s"tax-code.$taxCode"))),
        value = Value(Text(amount.toPoundSterlingString)),
        actions = reclaimAmountChangeCallOpt.map(reclaimAmountChangeCall =>
          Actions(
            items = Seq(
              ActionItem(
                href = reclaimAmountChangeCall(securityDepositId, taxCode).url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(
                  messages(
                    "check-claim.securities.hidden.tax-code",
                    messages(s"select-duties.duty.$taxCode"),
                    securityDepositId
                  )
                )
              )
            )
          )
        )
      )
    } ++ Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.claim-total"))),
        value = Value(Text(reclaims.values.sum.toPoundSterlingString))
      )
    )
  )
}
