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

import cats.implicits.catsSyntaxOptionId
import play.api.i18n.Messages
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.toDisplayDate
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Value

object CheckYourAnswersPdfHelper {

  def renderDeclarationDetails(
    declaration: DisplayDeclaration,
    key: String
  )(implicit
    messages: Messages
  ): SummaryList                                                                                         =
    SummaryList(
      Seq(
        declaration.getMaybeLRN.map(lrn =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.lrn-label"))),
            value = Value(Text(lrn))
          )
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.import-date-label"))),
          value = Value(Text(toDisplayDate(declaration.displayResponseDetail.acceptanceDate)))
        ).some,
        declaration.consigneeName.map(name =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.importer-name-label"))),
            value = Value(Text(name))
          )
        ),
        declaration.consigneeEmail.map(email =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.importer-email-label"))),
            value = Value(Text(email))
          )
        ),
        declaration.consigneeTelephone.map(telephone =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.importer-telephone-label"))),
            value = Value(Text(telephone))
          )
        ),
        declaration.consigneeAddress.map(address =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.importer-address-label"))),
            value = Value(HtmlContent(address))
          )
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.declarant-name-label"))),
          value = Value(Text(declaration.declarantName))
        ).some,
        declaration.declarantContactAddress.map(address =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.declarant-address-label"))),
            value = Value(HtmlContent(address))
          )
        )
      ).flatMap(_.toList)
    )
  def renderContactInformation(
    claimantInformation: ClaimantInformation,
    basisForClaim: BasisOfOverpaymentClaim,
    additionDetails: String,
    key: String
  )(implicit
    messages: Messages
  ): SummaryList = {
    val contactData = ClaimantInformationSummary.getContactDataHtml(claimantInformation)
    val addressData = ClaimantInformationSummary.getAddressDataHtml(claimantInformation)
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.contact-details"))),
          value = Value(HtmlContent(HtmlFormat.fill(contactData)))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.contact-address"))),
          value = Value(HtmlContent(HtmlFormat.fill(addressData)))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.basis-of-claim"))),
          value = Value(
            HtmlContent(messages(s"select-basis-for-claim.reason.$basisForClaim"))
          )
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.additional-details"))),
          value = Value(
            HtmlContent(additionDetails)
          )
        )
      )
    )
  }
  def renderReimbursements(reimbursements: Seq[Reimbursement])(implicit messages: Messages): SummaryList =
    SummaryList(
      reimbursements
        .map { summary =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"tax-code.${summary.taxCode}"))),
            value = Value(Text(summary.amount.toPoundSterlingString))
          )
        } ++
        Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.pdf.total"))),
            value = Value(Text(reimbursements.map(_.amount).sum.toPoundSterlingString))
          )
        )
    )

  def renderRepaymentDetails(payee: PayeeType, method: ReimbursementMethod)(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.payee-type.label"))),
          value = Value(HtmlContent(messages(PayeeTypeSummary.answerKey("check-your-answers.payee-type", payee))))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.repayment-method.label"))),
          value = Value(
            HtmlContent(messages(ReimbursementMethodSummary.answerKey("check-your-answers.repayment-method", method)))
          )
        )
      )
    )

  def renderClaimDetails(caseNumber: String, mrn: MRN, amountRequested: String)(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("confirmation-of-submission.reimbursement-amount"))),
          value = Value(Text(amountRequested))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages("confirmation-of-submission.claim-reference"))),
          value = Value(Text(caseNumber))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages("confirmation-of-submission.mrn"))),
          value = Value(Text(mrn.value))
        )
      )
    )

}
