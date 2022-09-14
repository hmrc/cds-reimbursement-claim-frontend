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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OrdinalNumeral
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.ActionItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Actions
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow

import scala.collection.immutable.SortedMap

object SecuritiesReclaimDetailsSummary {

  def apply(
    securityDepositId: String,
    reclaims: SortedMap[TaxCode, BigDecimal],
    declaration: DisplayDeclaration,
    key: String,
    fullAmountChangeCall: String => Call,
    dutiesSelectionChangeCall: String => Call,
    reclaimAmountChangeCall: (String, TaxCode) => Call
  )(implicit
    messages: Messages
  ): SummaryList = SummaryList(
    Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.claim-full-amount.label"))),
        value = Value(
          Text(
            messages(
              if (declaration.isFullSecurityAmount(securityDepositId, reclaims.values.sum))
                s"$key.claim-full-amount.yes"
              else
                s"$key.claim-full-amount.no"
            )
          )
        ),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = fullAmountChangeCall(securityDepositId).url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.claim-full-amount.label"))
              )
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.duties-selected.label"))),
        value = Value(
          HtmlContent(
            reclaims.keys
              .map(taxCode => messages(s"tax-code.${taxCode.value}"))
              .mkString("<br>")
          )
        ),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = dutiesSelectionChangeCall(securityDepositId).url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.duties-selected.label"))
              )
            )
          )
        )
      )
    ) ++ reclaims.zipWithIndex.map { case ((taxCode, amount), index) =>
      SummaryListRow(
        key = Key(HtmlContent(messages(s"tax-code.${taxCode.value}"))),
        value = Value(Text(amount.toPoundSterlingString)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = reclaimAmountChangeCall(securityDepositId, taxCode).url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(s"${OrdinalNumeral(index).capitalize} MRN: ${TaxCodes
                  .findTaxType(taxCode)} Duty ${taxCode.value} - ${messages(s"select-duties.duty.$taxCode")}")
              )
            )
          )
        )
      )
    }.toSeq ++ Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.claim-total"))),
        value = Value(Text(reclaims.values.sum.toPoundSterlingString))
      )
    )
  )
}
