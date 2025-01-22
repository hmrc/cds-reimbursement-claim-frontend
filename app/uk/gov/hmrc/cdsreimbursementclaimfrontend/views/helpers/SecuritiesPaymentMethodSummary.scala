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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

object SecuritiesPaymentMethodSummary {

  def apply(
    displayDeclaration: DisplayDeclaration,
    securityDepositIds: Set[String],
    key: String
  )(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    if (displayDeclaration.isAllSelectedSecuritiesEligibleForDifferentRepaymentMethods(securityDepositIds)) {
      Seq.empty[SummaryListRow]
    } else
      maybeLabelKey(displayDeclaration, securityDepositIds) match {
        case Some(labelKey) =>
          Seq(
            SummaryListRow(
              key = Key(Text(messages(s"check-your-answers.payment-method.label"))),
              value = Value(Text(messages(labelKey)))
            )
          )
        case None           => Seq.empty
      }

  private def maybeLabelKey(displayDeclaration: DisplayDeclaration, securityDepositIds: Set[String]): Option[String] =
    if (displayDeclaration.isAllSelectedSecuritiesEligibleForGuaranteePayment(securityDepositIds)) {
      Some(s"check-your-answers.payment-method.guarantee")
    } else if (displayDeclaration.isAllSelectedSecuritiesEligibleForBankAccount(securityDepositIds)) {
      Some(s"check-your-answers.payment-method.bt")
    } else if (displayDeclaration.isAllSelectedSecuritiesEligibleForCashAccount(securityDepositIds)) {
      Some(s"check-your-answers.payment-method.cash-account")
    } else if (displayDeclaration.isAllSelectedSecuritiesEligibleForDefermentAccount(securityDepositIds)) {
      Some(s"check-your-answers.payment-method.duty-deferment")
    } else {
      None
    }
}
