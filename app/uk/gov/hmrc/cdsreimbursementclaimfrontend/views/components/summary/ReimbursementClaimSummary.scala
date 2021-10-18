/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.ReimbursementClaimAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.{DutyType, ReimbursementClaimAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MoneyUtils
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

class ReimbursementClaimSummary {

  def renderPreCyaSummary(key: String, dutyType: DutyType, r: ReimbursementClaimAnswer)(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(rows =
      r.reimbursementClaims(dutyType)
        .map { taxCodeToReimburseClaim =>
          SummaryListRow(
            key = Key(Text(messages(s"select-duties.duty.${taxCodeToReimburseClaim._1.value}.row.key"))),
            value = Value(Text(MoneyUtils.formatAmountOfMoneyWithPoundSign(taxCodeToReimburseClaim._2.refundTotal))),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = reimbursementRoutes.SelectDutyCodesController.showDutyCodes(dutyType).url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"$key.$taxCodeToReimburseClaim.row.key"))
                  )
                )
              )
            )
          )
        }
        .toList ::: SummaryListRow(
        key = Key(Text(messages(s"$key.$dutyType.row.key"))),
        value = Value(
          Text(
            MoneyUtils.formatAmountOfMoneyWithPoundSign(
              r.reimbursementClaims(dutyType).subtotal
            )
          )
        ),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = reimbursementRoutes.SelectDutyCodesController.showDutyCodes(dutyType).url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages(s"$key.$dutyType.row.key"))
              )
            )
          )
        )
      ) :: Nil
    )
}
