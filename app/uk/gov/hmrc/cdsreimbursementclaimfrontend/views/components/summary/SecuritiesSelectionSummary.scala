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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BigDecimalOps, TaxCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.govukfrontend.views.Aliases.{Text, Value}
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

import scala.collection.immutable.SortedMap

object SecuritiesSelectionSummary {

  def apply(
    securitiesReclaims: SortedMap[String, SortedMap[TaxCode, BigDecimal]],
    declaration: DisplayDeclaration,
    key: String,
    changeCall: String => Call
  )(implicit
    messages: Messages
  ): SummaryList = SummaryList(
    declaration.getSecurityDepositIds.getOrElse(Nil).map { securityDepositId =>
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.claim-for-security.label", securityDepositId))),
        value = Value(
          Text(
            messages(
              if (securitiesReclaims.contains(securityDepositId))
                s"$key.claim-for-security.yes"
              else
                s"$key.claim-for-security.no"
            )
          )
        ),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = changeCall(securityDepositId).url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.claim-for-security.label"))
              )
            )
          )
        )
      )
    } ++ Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.claim-for-security.total"))),
        value = Value(
          Text(declaration.getTotalSecuritiesAmountFor(securitiesReclaims.keySet).toPoundSterlingString)
        )
      )
    )
  )
}
