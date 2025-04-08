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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.Aliases.Value
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

import scala.collection.immutable.SortedMap
import scala.util.Try

object SecuritiesSelectionSummary {

  def apply(
    correctedAmounts: SortedMap[String, SortedMap[TaxCode, BigDecimal]],
    declaration: DisplayDeclaration,
    key: String,
    changeCallOpt: Option[String => Call],
    showTotalSecuritiesPaidAmount: Boolean = false
  )(implicit
    messages: Messages
  ): SummaryList = {
    val numberOfSecurities = declaration.getSecurityDepositIds.map(_.size).getOrElse(0)
    val total: BigDecimal  = declaration.getTotalSecuritiesAmountFor(correctedAmounts.keySet)
    SummaryList(
      if Try(total.toIntExact == 0).getOrElse(false) then Nil
      else
        declaration.getSecurityDepositIds.getOrElse(Nil).zipWithIndex.map { case (securityDepositId, index) =>
          SummaryListRow(
            key = Key(
              HtmlContent(
                if numberOfSecurities > 1
                then messages(s"$key.claim-for-security.label", index + 1, numberOfSecurities)
                else messages(s"$key.claim-for-security.single.label")
              )
            ),
            value = Value(
              Text(
                messages(
                  if correctedAmounts.contains(securityDepositId) then s"$key.claim-for-security.yes"
                  else s"$key.claim-for-security.no"
                )
              )
            ),
            actions = changeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall(securityDepositId).url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(
                      if numberOfSecurities > 1
                      then messages(s"$key.claim-for-security.label", index + 1, numberOfSecurities)
                      else messages(s"$key.claim-for-security.single.label")
                    )
                  )
                )
              )
            )
          )
        }
          ++ Seq(
            SummaryListRow(
              key = Key(HtmlContent(messages(s"$key.claim-for-security.total"))),
              value = Value(
                Text(total.toPoundSterlingString)
              )
            )
          )
          ++ (if showTotalSecuritiesPaidAmount then
                Seq(
                  SummaryListRow(
                    key = Key(HtmlContent(messages(s"$key.claim-for-security.paid-total"))),
                    value = Value(
                      Text(declaration.getTotalSecuritiesPaidAmountFor(correctedAmounts.keySet).toPoundSterlingString)
                    )
                  )
                )
              else Seq.empty)
    )
  }
}
