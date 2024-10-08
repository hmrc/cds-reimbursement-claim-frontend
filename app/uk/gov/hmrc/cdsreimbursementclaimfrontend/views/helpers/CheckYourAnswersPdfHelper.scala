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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.parseYyyyMmDdToDisplayDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.toDisplayDate
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Value
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.{routes => overpaymentsSingleRoute}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.{routes => overpaymentsMultipleRoute}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.{routes => securitiesRoute}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.Paragraph

import scala.collection.immutable.SortedMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.{routes => overpaymentsScheduledRoute}

object CheckYourAnswersPdfHelper {

  def renderDeclarationDetails(
    declaration: DisplayDeclaration,
    key: String
  )(implicit
    messages: Messages
  ): SummaryList =
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

  def renderDeclarationDetailsForSecurities(
    declaration: DisplayDeclaration,
    claim: SecuritiesJourney.Output,
    key: String
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.pdf.movement-reference-number"))),
          value = Value(Text(declaration.displayResponseDetail.declarationId))
        ).some,
        declaration.getMaybeLRN.map(lrn =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.lrn-label"))),
            value = Value(Text(lrn))
          )
        ),
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
        ),
        declaration.getReasonForSecurity.map { reason =>
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.reason-for-security-label"))),
            value = Value(Text(messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(reason)}")))
          )
        },
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.pdf.acceptance-date"))),
          value = Value(Text(toDisplayDate(declaration.displayResponseDetail.acceptanceDate)))
        ).some,
        declaration.displayResponseDetail.btaDueDate.map { date =>
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.pdf.tax-collection-date"))),
            value = Value(Text(parseYyyyMmDdToDisplayDate(date)))
          )
        }
      ).flatMap(_.toList) ++
        declaration.getSecurityDepositIds.getOrElse(Nil).map { securityDepositId =>
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.claim-for-security.label", securityDepositId))),
            value = Value(
              Text(
                messages(
                  if (claim.securitiesReclaims.contains(securityDepositId))
                    "check-your-answers.claim-for-security.yes"
                  else
                    "check-your-answers.claim-for-security.no"
                )
              )
            )
          )
        } ++ Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.pdf.claim-for-security.total"))),
            value = Value(
              Text(declaration.getTotalSecuritiesAmountFor(claim.securitiesReclaims.keySet).toPoundSterlingString)
            )
          ).some,
          getPaymentMethod(declaration, claim)
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

  def renderContactInformationCommon(
    claimantInformation: ClaimantInformation,
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
        )
      )
    )
  }

  def renderBasisOfClaimDetails(
    basisForClaim: BasisOfOverpaymentClaim,
    additionDetails: String,
    key: String
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
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

  def renderClaimDetails(
    caseNumber: String,
    mrn: MRN,
    amountRequested: String,
    mrnKey: String = "confirmation-of-submission.mrn"
  )(implicit
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
          key = Key(HtmlContent(messages(mrnKey))),
          value = Value(Text(mrn.value))
        )
      )
    )

  def renderClaimDetailsForMultiple(caseNumber: String, amountRequested: BigDecimal, mrns: Seq[MRN])(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("confirmation-of-submission.reimbursement-amount"))),
          value = Value(Text(amountRequested.toPoundSterlingString))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages("confirmation-of-submission.claim-reference"))),
          value = Value(Text(caseNumber))
        )
      ) ++ mrns.zipWithIndex.map { case (mrn, index) =>
        SummaryListRow(
          key = Key(
            HtmlContent(OrdinalNumberMrnHelper(index + 1))
          ),
          value = Value(Text(mrn.value))
        )
      }
    )

  def renderClaimTotalsForMultiple(reimbursementClaims: Map[MRN, Map[TaxCode, BigDecimal]], key: String)(implicit
    messages: Messages
  ): SummaryList = {

    val totalAmount: BigDecimal =
      reimbursementClaims.flatMap(_._2.values).sum

    SummaryList(rows =
      reimbursementClaims.toSeq
        .map { case (mrn, claims) =>
          SummaryListRow(
            key = Key(Text(mrn.value)),
            value = Value(Text(claims.values.sum.toPoundSterlingString))
          )
        } ++
        Seq(
          SummaryListRow(
            key = Key(HtmlContent(messages(s"$key.multiple.total"))),
            value = Value(Text(totalAmount.toPoundSterlingString))
          )
        )
    )
  }

  def renderSecurityDeposit(
    securityDepositId: String,
    reclaims: SortedMap[TaxCode, BigDecimal],
    declaration: DisplayDeclaration,
    key: String
  )(implicit messages: Messages): SummaryList = SummaryList(
    Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages(s"$key.claim-for-security.claim-full-amount.label"))),
        value = Value(
          Text(
            messages(
              if (declaration.isFullSecurityAmount(securityDepositId, reclaims.values.sum))
                "check-your-answers.claim-for-security.claim-full-amount.yes"
              else
                "check-your-answers.claim-for-security.claim-full-amount.no"
            )
          )
        )
      ),
      SummaryListRow(
        key = Key(HtmlContent(messages("check-your-answers.pdf.selected-charges"))),
        value = Value(
          HtmlContent(
            reclaims.keys.toList.sorted
              .map(taxCode => messages(s"tax-code.$taxCode"))
              .mkString("<br>")
          )
        )
      )
    ) ++ reclaims.toList.sorted.zipWithIndex.map { case ((taxCode, amount), _) =>
      SummaryListRow(
        key = Key(HtmlContent(messages(s"tax-code.$taxCode"))),
        value = Value(Text(amount.toPoundSterlingString))
      )
    } ++ Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages("check-your-answers.pdf.total"))),
        value = Value(Text(reclaims.values.sum.toPoundSterlingString))
      )
    )
  )

  def renderBankDetails(bankAccountDetails: BankAccountDetails, key: String)(implicit messages: Messages): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.account-name.label"))),
          value = Value(Text(bankAccountDetails.accountName.value))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.sort-code.label"))),
          value = Value(Text(bankAccountDetails.sortCode.masked))
        ),
        SummaryListRow(
          key = Key(HtmlContent(messages(s"$key.account-number.label"))),
          value = Value(Text(bankAccountDetails.accountNumber.masked))
        )
      )
    )

  def renderEvidenceForSecurities(answers: Seq[EvidenceDocument])(implicit messages: Messages): SummaryList =
    SummaryList(
      answers.map(document =>
        SummaryListRow(
          key = Key(Text(messages(s"choose-file-type.file-type.${document.documentType}"))),
          value = Value(
            HtmlContent(
              Paragraph(
                document.fileName,
                messages(s"choose-file-type.file-type.${document.documentType}")
              ).toString
            )
          )
        )
      )
    )

  def renderScheduledDocument(fileName: String)(implicit messages: Messages): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.scheduled-document.label"))),
          value = Value(Text(fileName))
        )
      )
    )

  def getPdfUrl(journey: JourneyBase): String = journey match {
    case journey: OverpaymentsSingleJourney    => overpaymentsSingleRoute.CheckYourAnswersController.showPdf.url
    case journey: OverpaymentsMultipleJourney  => overpaymentsMultipleRoute.CheckYourAnswersController.showPdf.url
    case journey: SecuritiesJourney            => securitiesRoute.CheckYourAnswersController.showPdf.url
    case journey: OverpaymentsScheduledJourney => overpaymentsScheduledRoute.CheckYourAnswersController.showPdf.url
  }

  private def getPaymentMethod(displayDeclaration: DisplayDeclaration, claim: SecuritiesJourney.Output)(implicit
    messages: Messages
  ) =
    if (
      displayDeclaration.isAllSelectedSecuritiesEligibleForDifferentRepaymentMethods(claim.securitiesReclaims.keySet)
    ) {
      None
    } else {
      SummaryListRow(
        key = Key(Text(messages(s"check-your-answers.payment-method.label"))),
        value = Value(
          Text(
            messages(
              if (
                displayDeclaration.isAllSelectedSecuritiesEligibleForGuaranteePayment(claim.securitiesReclaims.keySet)
              ) {
                s"check-your-answers.payment-method.guarantee"
              } else if (
                displayDeclaration.isAllSelectedSecuritiesEligibleForBankAccount(claim.securitiesReclaims.keySet)
              ) {
                s"check-your-answers.payment-method.bt"
              } else if (
                displayDeclaration.isAllSelectedSecuritiesEligibleForCashAccount(claim.securitiesReclaims.keySet)
              ) {
                s"check-your-answers.payment-method.cash-account"
              } else if (
                displayDeclaration.isAllSelectedSecuritiesEligibleForDefermentAccount(claim.securitiesReclaims.keySet)
              ) {
                s"check-your-answers.payment-method.duty-deferment"
              } else {
                s"check-your-answers.payment-method.unknown"
              }
            )
          )
        )
      ).some
    }
}
