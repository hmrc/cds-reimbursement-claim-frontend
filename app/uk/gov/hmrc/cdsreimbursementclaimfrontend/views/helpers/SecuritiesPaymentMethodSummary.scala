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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

object SecuritiesPaymentMethodSummary {

  def apply(
    importDeclaration: ImportDeclaration,
    securityDepositIds: Set[String],
    key: String
  )(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    if (importDeclaration.isAllSelectedSecuritiesEligibleForDifferentRepaymentMethods(securityDepositIds)) {
      Seq.empty[SummaryListRow]
    } else
      maybeLabelKey(importDeclaration, securityDepositIds) match {
        case Some(labelKey) =>
          Seq(
            SummaryListRow(
              key = Key(Text(messages(s"check-your-answers.payment-method.label"))),
              value = Value(Text(messages(labelKey)))
            )
          )
        case None           => Seq.empty
      }

  private def maybeLabelKey(importDeclaration: ImportDeclaration, securityDepositIds: Set[String]): Option[String] =
    if (importDeclaration.isAllSelectedSecuritiesEligibleForGuaranteePayment(securityDepositIds)) {
      Some(s"check-your-answers.payment-method.guarantee")
    } else if (importDeclaration.isAllSelectedSecuritiesEligibleForBankAccount(securityDepositIds)) {
      Some(s"check-your-answers.payment-method.bt")
    } else if (importDeclaration.isAllSelectedSecuritiesEligibleForCashAccount(securityDepositIds)) {
      Some(s"check-your-answers.payment-method.cash-account")
    } else if (importDeclaration.isAllSelectedSecuritiesEligibleForDefermentAccount(securityDepositIds)) {
      Some(s"check-your-answers.payment-method.duty-deferment")
    } else {
      None
    }
}
