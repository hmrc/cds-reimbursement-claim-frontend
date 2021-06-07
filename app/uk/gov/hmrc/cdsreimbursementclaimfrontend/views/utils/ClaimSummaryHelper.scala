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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils

import cats.data.NonEmptyList
import play.api.i18n.{Lang, Langs, MessagesApi}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{ClaimsAnswer, TaxCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.finance.MoneyUtils
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

import javax.inject.{Inject, Singleton}

@Singleton
class ClaimSummaryHelper @Inject() (implicit langs: Langs, messages: MessagesApi) {

  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "check-claim-summary"

  def makeClaimSummary(claims: ClaimsAnswer): NonEmptyList[SummaryListRow] =
    if (isUKTax(claims.map(t => t.taxCode).toString())) {
      makeClaimSummaryRows(claims) :+ makeClaimTotalRow(claims)
    } else if (isEuTax(claims.map(t => t.taxCode).toString())) {
      makeClaimSummaryRows(claims) :+ makeClaimTotalRow(claims)
    } else
      makeClaimSummaryRows(claims) :+ makeClaimTotalRow(claims)

  def claimSummary(claims: ClaimsAnswer): NonEmptyList[SummaryListRow] =
    makeClaimSummaryRows(claims) :+ makeClaimTotalRow(claims) // copy of list with element appended

  //make into hashset
  //for loop over all claims
  // check for each category
  // if uk -> Make summary + total
  // if eu -> Make summary + total
  // if excise -> Make summary + total
  // Make overall total summary row

  def isUKTax(taxCode: String): Boolean =
    TaxCode.listOfUKTaxCodes.map(t => t.toString()).exists(p => p.contains(taxCode))

  def isEuTax(taxCode: String): Boolean =
    TaxCode.listOfEUTaxCodes.map(t => t.toString()).exists(p => p.contains(taxCode))

  def isExciseTax(taxCode: String): Boolean =
    TaxCode.listOfUKExciseCodes.map(t => t.toString()).exists(p => p.contains(taxCode))

  def makeClaimSummaryRows(claims: ClaimsAnswer): NonEmptyList[SummaryListRow] =
    claims.map { claim =>
      SummaryListRow(
        key = Key(Text(messages(s"select-duties.duty.${claim.taxCode}")(lang))),
        value = Value(Text(MoneyUtils.formatAmountOfMoneyWithPoundSign(claim.claimAmount)))
      )
    }

  def makeClaimTotalRow(claims: ClaimsAnswer): SummaryListRow =
    SummaryListRow(
      key = Key(Text(messages(s"$key.total")(lang))),
      value = Value(Text(MoneyUtils.formatAmountOfMoneyWithPoundSign(claims.total)))
    )

}
