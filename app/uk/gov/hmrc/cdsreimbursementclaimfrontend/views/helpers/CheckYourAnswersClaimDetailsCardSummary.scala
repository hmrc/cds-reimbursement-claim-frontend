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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.routes as overpaymentsMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.routes as overpaymentsScheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.routes as overpaymentsSingleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple.routes as rejectedGoodsMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.routes as rejectedGoodsScheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.routes as rejectedGoodsSingleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.routes as securitiesRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.NewEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.EndUseRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.InwardProcessingRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

object CheckYourAnswersClaimDetailsCardSummary {

  def render(
    claimType: String,
    isPrintView: Boolean,
    mrnRowsOpt: Option[Seq[SummaryListRow]],
    claimSummaryDocumentOpt: Option[EvidenceDocument] = None,
    basisOfClaimRows: Seq[SummaryListRow],
    disposalMethodOpt: Option[MethodOfDisposal] = None,
    duplicateMovementReferenceNumber: Option[MRN] = None,
    claimSummaryDocumentChangeCallOpt: Option[Call] = None,
    disposalMethodChangeCallOpt: Option[Call] = None,
    newEoriAndDanOpt: Option[NewEoriAndDan] = None,
    additionalDetails: String,
    additionalDetailsChangeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList = {
    SummaryList(
      Seq(
        mrnRowsOpt.getOrElse(None),
        claimSummaryDocumentOpt.map { doc =>
          SummaryListRow(
            key = Key(Text(messages("check-your-answers.scheduled.claim-summary-document"))),
            value = Value(
              HtmlContent(
                if (isPrintView) {
                  doc.fileName
                } else {
                  doc.previewUrl
                    .map(previewUrl => s"<a href='$previewUrl' target='_blank' class='govuk-link'>${doc.fileName}</a>")
                    .getOrElse(doc.fileName)
                }
              )
            ),
            actions = claimSummaryDocumentChangeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages("check-your-answers.scheduled.claim-summary-document"))
                  )
                )
              )
            )
          )
        },
        basisOfClaimRows,
        disposalMethodOpt.map { disposalMethod =>
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.disposal-method.label"))),
            value = Value(
              Text(messages(s"select-method-of-disposal.rejected-goods.method.$disposalMethod"))
            ),
            actions = disposalMethodChangeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages("check-your-answers.disposal-method.label"))
                  )
                )
              )
            )
          )
        },
        duplicateMovementReferenceNumber.map { duplicateMrn =>
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.duplicate-mrn"))),
            value = Value(
              Text(duplicateMrn.value)
            ),
            actions =
              if isPrintView then None
              else
                Some(
                  Actions(
                    items = Seq(
                      ActionItem(
                        href = overpaymentsSingleRoutes.EnterDuplicateMovementReferenceNumberController.show.url,
                        content = Text(messages("cya.change")),
                        visuallyHiddenText = Some(messages("check-your-answers.duplicate-mrn"))
                      )
                    )
                  )
                )
          )
        },
        newEoriAndDanOpt.map { newEoriAndDan =>
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.correct-eori"))),
            value = Value(
              Text(newEoriAndDan.eori.value)
            ),
            actions =
              if isPrintView then None
              else
                Some(
                  Actions(
                    items = Seq(
                      ActionItem(
                        href = getNewEoriPageUrl(claimType),
                        content = Text(messages("cya.change")),
                        visuallyHiddenText = Some(messages("check-your-answers.correct-eori"))
                      )
                    )
                  )
                )
          )
        },
        newEoriAndDanOpt.map { newEoriAndDan =>
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.correct-dan"))),
            value = Value(
              Text(newEoriAndDan.dan)
            ),
            actions =
              if isPrintView then None
              else
                Some(
                  Actions(
                    items = Seq(
                      ActionItem(
                        href = getNewDanPageUrl(claimType),
                        content = Text(messages("cya.change")),
                        visuallyHiddenText = Some(messages("check-your-answers.new-dan"))
                      )
                    )
                  )
                )
          )
        },
        Some(
          SummaryListRow(
            key = Key(HtmlContent(messages("check-your-answers.additional-claim-information"))),
            value = Value(
              Text(additionalDetails)
            ),
            actions = additionalDetailsChangeCallOpt.map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages("check-your-answers.additional-claim-information"))
                  )
                )
              )
            )
          )
        )
      ).flatten
    )
  }

  private def getNewEoriPageUrl(claim: String): String = claim match {
    case "single"    => overpaymentsSingleRoutes.EnterNewEoriNumberController.show.url
    case "multiple"  => overpaymentsMultipleRoutes.EnterNewEoriNumberController.show.url
    case "scheduled" => overpaymentsScheduledRoutes.EnterNewEoriNumberController.show.url
  }

  private def getNewDanPageUrl(claim: String): String = claim match {
    case "single"    => overpaymentsSingleRoutes.EnterNewDanController.show.url
    case "multiple"  => overpaymentsMultipleRoutes.EnterNewDanController.show.url
    case "scheduled" => overpaymentsScheduledRoutes.EnterNewDanController.show.url
  }

  private def makeMrnRowsForSingle(mrn: MRN, mrnChangeCallOpt: Option[Call])(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages("check-your-answers.single.mrn-label-plaintext"))),
        value = Value(Text(mrn.value)),
        actions = mrnChangeCallOpt.map(changeCall =>
          Actions(
            items = Seq(
              ActionItem(
                href = changeCall.url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages("check-your-answers.single.mrn-label-plaintext"))
              )
            )
          )
        ),
        classes = "mrn-value"
      )
    )

  private def makeMrnRowsForMultiple(mrns: Seq[MRN], mrnChangeCallOpt: Option[Call])(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    mrns.zipWithIndex.map { case (mrn, index) =>
      SummaryListRow(
        key = Key(
          HtmlContent(OrdinalNumberMrnHelper(index + 1, index == 0))
        ),
        value = Value(Text(mrn.value)),
        actions = mrnChangeCallOpt
          .map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(OrdinalNumberMrnHelper(index + 1))
                )
              )
            )
          ),
        classes = "mrn-value"
      )
    }

  private def makeMrnRowsForScheduled(mrn: MRN, mrnChangeCallOpt: Option[Call])(implicit
    messages: Messages
  ): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages("check-your-answers.scheduled.mrn-label-plaintext"))),
        value = Value(Text(mrn.value)),
        actions = mrnChangeCallOpt.map(changeCall =>
          Actions(
            items = Seq(
              ActionItem(
                href = changeCall.url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(OrdinalNumberMrnHelper(1))
              )
            )
          )
        ),
        classes = "mrn-value"
      )
    )

  private def makeBasisOfClaimRowsForOverpayments(
    basisOfClaim: BasisOfOverpaymentClaim,
    basisOfClaimChangeCallOpt: Option[Call]
  )(implicit messages: Messages): Seq[SummaryListRow] =
    Seq(
      SummaryListRow(
        key = Key(HtmlContent(messages("check-your-answers.reason-for-claim"))),
        value = Value(
          HtmlContent(messages(s"select-basis-for-claim.reason.$basisOfClaim"))
        ),
        actions = basisOfClaimChangeCallOpt.map(changeCall =>
          Actions(
            items = Seq(
              ActionItem(
                href = changeCall.url,
                content = Text(messages("cya.change")),
                visuallyHiddenText = Some(messages("check-your-answers.reason-for-claim"))
              )
            )
          )
        )
      )
    )

  private def makeBasisOfClaimRowsForRejectedGoods(
    basisOfClaim: BasisOfRejectedGoodsClaim,
    basisOfClaimSpecialCircumstances: Option[String],
    basisOfClaimChangeCallOpt: Option[Call],
    basisOfClaimSpecialCircumstancesChangeCallOpt: Option[Call]
  )(implicit messages: Messages): Seq[SummaryListRow] =
    Seq(
      Some(
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.reason-for-claim"))),
          value = Value(
            HtmlContent(messages(s"select-basis-for-claim.rejected-goods.reason.$basisOfClaim"))
          ),
          actions = basisOfClaimChangeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages("check-your-answers.reason-for-claim"))
                )
              )
            )
          )
        )
      ),
      basisOfClaimSpecialCircumstances.map { specialCircumstances =>
        SummaryListRow(
          key = Key(HtmlContent(messages("check-your-answers.special-circumstances.details"))),
          value = Value(Text(specialCircumstances)),
          actions = basisOfClaimSpecialCircumstancesChangeCallOpt.map(changeCall =>
            Actions(
              items = Seq(
                ActionItem(
                  href = changeCall.url,
                  content = Text(messages("cya.change")),
                  visuallyHiddenText = Some(messages("check-your-answers.special-circumstances.details"))
                )
              )
            )
          )
        )
      }
    ).flatten

  def renderForSingle(claim: OverpaymentsSingleClaim.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      claimType = "single",
      isPrintView = isPrintView,
      mrnRowsOpt = Some(
        makeMrnRowsForSingle(
          claim.movementReferenceNumber,
          if !isPrintView then Some(overpaymentsSingleRoutes.EnterMovementReferenceNumberController.show) else None
        )
      ),
      basisOfClaimRows = makeBasisOfClaimRowsForOverpayments(
        claim.basisOfClaim,
        if !isPrintView then Some(overpaymentsSingleRoutes.BasisForClaimController.show) else None
      ),
      duplicateMovementReferenceNumber = claim.duplicateMovementReferenceNumber,
      newEoriAndDanOpt = claim.newEoriAndDan,
      additionalDetails = claim.additionalDetails,
      additionalDetailsChangeCallOpt =
        if !isPrintView then Some(overpaymentsSingleRoutes.EnterAdditionalDetailsController.show) else None
    )

  def renderForSingle(claim: RejectedGoodsSingleClaim.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      claimType = "single",
      isPrintView = isPrintView,
      mrnRowsOpt = Some(
        makeMrnRowsForSingle(
          claim.movementReferenceNumber,
          if !isPrintView then Some(rejectedGoodsSingleRoutes.EnterMovementReferenceNumberController.show) else None
        )
      ),
      basisOfClaimRows = makeBasisOfClaimRowsForRejectedGoods(
        claim.basisOfClaim,
        claim.basisOfClaimSpecialCircumstances,
        if !isPrintView then Some(rejectedGoodsSingleRoutes.BasisForClaimController.show) else None,
        if !isPrintView && claim.basisOfClaimSpecialCircumstances.isDefined then
          Some(rejectedGoodsSingleRoutes.EnterSpecialCircumstancesController.show)
        else None
      ),
      disposalMethodOpt = Some(claim.methodOfDisposal),
      disposalMethodChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsSingleRoutes.DisposalMethodController.show) else None,
      additionalDetails = claim.detailsOfRejectedGoods,
      additionalDetailsChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsSingleRoutes.EnterRejectedGoodsDetailsController.show) else None
    )

  def renderForMultiple(claim: OverpaymentsMultipleClaim.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      claimType = "multiple",
      isPrintView = isPrintView,
      mrnRowsOpt = Some(
        makeMrnRowsForMultiple(
          claim.movementReferenceNumbers,
          if !isPrintView then Some(overpaymentsMultipleRoutes.CheckMovementReferenceNumbersController.show) else None
        )
      ),
      basisOfClaimRows = makeBasisOfClaimRowsForOverpayments(
        claim.basisOfClaim,
        if !isPrintView then Some(overpaymentsMultipleRoutes.BasisForClaimController.show) else None
      ),
      newEoriAndDanOpt = claim.newEoriAndDan,
      additionalDetails = claim.additionalDetails,
      additionalDetailsChangeCallOpt =
        if !isPrintView then Some(overpaymentsMultipleRoutes.EnterAdditionalDetailsController.show) else None
    )

  def renderForMultiple(claim: RejectedGoodsMultipleClaim.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      claimType = "multiple",
      isPrintView = isPrintView,
      mrnRowsOpt = Some(
        makeMrnRowsForMultiple(
          claim.movementReferenceNumbers,
          if !isPrintView then Some(rejectedGoodsMultipleRoutes.CheckMovementReferenceNumbersController.show) else None
        )
      ),
      basisOfClaimRows = makeBasisOfClaimRowsForRejectedGoods(
        claim.basisOfClaim,
        claim.basisOfClaimSpecialCircumstances,
        if !isPrintView then Some(rejectedGoodsMultipleRoutes.BasisForClaimController.show) else None,
        if !isPrintView && claim.basisOfClaimSpecialCircumstances.isDefined then
          Some(rejectedGoodsMultipleRoutes.EnterSpecialCircumstancesController.show)
        else None
      ),
      disposalMethodOpt = Some(claim.methodOfDisposal),
      disposalMethodChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsMultipleRoutes.DisposalMethodController.show) else None,
      additionalDetails = claim.detailsOfRejectedGoods,
      additionalDetailsChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsMultipleRoutes.EnterRejectedGoodsDetailsController.show) else None
    )

  def renderForScheduled(claim: OverpaymentsScheduledClaim.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      claimType = "scheduled",
      isPrintView = isPrintView,
      mrnRowsOpt = Some(
        makeMrnRowsForScheduled(
          claim.movementReferenceNumber,
          if !isPrintView then Some(overpaymentsScheduledRoutes.EnterMovementReferenceNumberController.show) else None
        )
      ),
      claimSummaryDocumentOpt = Some(claim.scheduledDocument),
      basisOfClaimRows = makeBasisOfClaimRowsForOverpayments(
        claim.basisOfClaim,
        if !isPrintView then Some(overpaymentsSingleRoutes.BasisForClaimController.show) else None
      ),
      newEoriAndDanOpt = claim.newEoriAndDan,
      additionalDetails = claim.additionalDetails,
      additionalDetailsChangeCallOpt =
        if !isPrintView then Some(overpaymentsScheduledRoutes.EnterAdditionalDetailsController.show) else None,
      claimSummaryDocumentChangeCallOpt =
        if !isPrintView then Some(overpaymentsScheduledRoutes.UploadMrnListController.show) else None
    )

  def renderForScheduled(claim: RejectedGoodsScheduledClaim.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      claimType = "scheduled",
      isPrintView = isPrintView,
      mrnRowsOpt = Some(
        makeMrnRowsForScheduled(
          claim.movementReferenceNumber,
          if !isPrintView then Some(rejectedGoodsScheduledRoutes.EnterMovementReferenceNumberController.show) else None
        )
      ),
      claimSummaryDocumentOpt = Some(claim.scheduledDocument),
      basisOfClaimRows = makeBasisOfClaimRowsForRejectedGoods(
        claim.basisOfClaim,
        claim.basisOfClaimSpecialCircumstances,
        if !isPrintView then Some(rejectedGoodsScheduledRoutes.BasisForClaimController.show) else None,
        if !isPrintView && claim.basisOfClaimSpecialCircumstances.isDefined then
          Some(rejectedGoodsScheduledRoutes.EnterSpecialCircumstancesController.show)
        else None
      ),
      disposalMethodOpt = Some(claim.methodOfDisposal),
      disposalMethodChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsScheduledRoutes.DisposalMethodController.show) else None,
      additionalDetails = claim.detailsOfRejectedGoods,
      additionalDetailsChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsScheduledRoutes.EnterRejectedGoodsDetailsController.show) else None,
      claimSummaryDocumentChangeCallOpt =
        if !isPrintView then Some(rejectedGoodsScheduledRoutes.UploadMrnListController.show) else None
    )

  def renderForSecurities(
    claim: SecuritiesClaim.Output,
    displayDeclarationOpt: Option[DisplayDeclaration],
    isPrintView: Boolean
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList.optionally(
      Some(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"check-your-answers.securities.mrn-label-plaintext"))),
          value = Value(Text(claim.movementReferenceNumber.value)),
          actions = (if (isPrintView) None else Some(securitiesRoutes.EnterMovementReferenceNumberController.show))
            .map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText = Some(messages(s"check-your-answers.securities.mrn-label-plaintext.hidden"))
                  )
                )
              )
            ),
          classes = "mrn-value"
        )
      ),
      Some(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"check-your-answers.securities.reason-for-security-label"))),
          value = Value(
            Text(
              messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(claim.reasonForSecurity)}")
            )
          ),
          actions = (if (isPrintView) None else Some(securitiesRoutes.ChooseReasonForSecurityController.show))
            .map(changeCall =>
              Actions(
                items = Seq(
                  ActionItem(
                    href = changeCall.url,
                    content = Text(messages("cya.change")),
                    visuallyHiddenText =
                      Some(messages(s"check-your-answers.securities.reason-for-security-label.hidden"))
                  )
                )
              )
            )
        )
      ),
      claim.reasonForSecurity.match {
        case EndUseRelief | InwardProcessingRelief => None
        case _                                     =>
          displayDeclarationOpt.flatMap(declaration =>
            claim.securitiesReclaims.headOption.map((securityDepositId, reclaims) =>
              SummaryListRow(
                key = Key(HtmlContent(messages(s"check-your-answers.securities.claim-full-amount.label"))),
                value = Value(
                  Text(
                    messages(
                      if declaration.isFullSecurityAmount(securityDepositId, reclaims.values.sum) then
                        s"check-your-answers.claim-for-security.claim-full-amount.yes"
                      else s"check-your-answers.claim-for-security.claim-full-amount.no"
                    )
                  )
                ),
                actions = (if (isPrintView) None
                           else Some(securitiesRoutes.ConfirmSingleDepositRepaymentController.show))
                  .map(changeCall =>
                    Actions(
                      items = Seq(
                        ActionItem(
                          href = changeCall.url,
                          content = Text(messages("cya.change")),
                          visuallyHiddenText = Some(messages(s"check-your-answers.securities.claim-full-amount.hidden"))
                        )
                      )
                    )
                  )
              )
            )
          )
      }
    )
}
