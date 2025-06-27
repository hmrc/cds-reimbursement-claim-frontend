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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.routes as multipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.routes as scheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.routes as singleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.NewEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

object CheckYourAnswersClaimDetailsCardSummary {

  def render(
    journeyType: String,
    mrnRowsOpt: Option[Seq[SummaryListRow]],
    claimSummaryDocumentOpt: Option[EvidenceDocument] = None,
    basisOfClaim: BasisOfOverpaymentClaim,
    duplicateMovementReferenceNumber: Option[MRN] = None,
    claimSummaryDocumentChangeCallOpt: Option[Call] = None,
    newEoriAndDanOpt: Option[NewEoriAndDan],
    additionalDetails: String,
    basisOfClaimChangeCallOpt: Option[Call],
    additionalDetailsChangeCallOpt: Option[Call]
  )(implicit
    messages: Messages
  ): SummaryList = {
    val isPrintView =
      additionalDetailsChangeCallOpt.isEmpty && basisOfClaimChangeCallOpt.isEmpty
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
        Some(
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
        ),
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
                        href = singleRoutes.EnterDuplicateMovementReferenceNumberController.show.url,
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
                        href = getNewEoriPageUrl(journeyType),
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
                        href = getNewDanPageUrl(journeyType),
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

  private def getNewEoriPageUrl(journey: String): String = journey match {
    case "single"    => singleRoutes.EnterNewEoriNumberController.show.url
    case "multiple"  => multipleRoutes.EnterNewEoriNumberController.show.url
    case "scheduled" => scheduledRoutes.EnterNewEoriNumberController.show.url
  }

  private def getNewDanPageUrl(journey: String): String = journey match {
    case "single"    => singleRoutes.EnterNewDanController.show.url
    case "multiple"  => multipleRoutes.EnterNewDanController.show.url
    case "scheduled" => scheduledRoutes.EnterNewDanController.show.url
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

  private def makeMrnRowsForMultiple(mrns: Seq[MRN], mrnChangeCallOpt: Option[Int => Call])(implicit
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
                  href = changeCall(index + 1).url,
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

  def renderForSingle(claim: OverpaymentsSingleJourney.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      journeyType = "single",
      mrnRowsOpt = Some(
        makeMrnRowsForSingle(
          claim.movementReferenceNumber,
          if !isPrintView then Some(singleRoutes.EnterMovementReferenceNumberController.show) else None
        )
      ),
      basisOfClaim = claim.basisOfClaim,
      duplicateMovementReferenceNumber = claim.duplicateMovementReferenceNumber,
      newEoriAndDanOpt = claim.newEoriAndDan,
      additionalDetails = claim.additionalDetails,
      basisOfClaimChangeCallOpt = if !isPrintView then Some(singleRoutes.BasisForClaimController.show) else None,
      additionalDetailsChangeCallOpt =
        if !isPrintView then Some(singleRoutes.EnterAdditionalDetailsController.show) else None
    )

  def renderForMultiple(claim: OverpaymentsMultipleJourney.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      journeyType = "multiple",
      mrnRowsOpt = Some(
        makeMrnRowsForMultiple(
          claim.movementReferenceNumbers,
          if !isPrintView then Some(multipleRoutes.EnterMovementReferenceNumberController.show) else None
        )
      ),
      basisOfClaim = claim.basisOfClaim,
      newEoriAndDanOpt = claim.newEoriAndDan,
      additionalDetails = claim.additionalDetails,
      basisOfClaimChangeCallOpt = if !isPrintView then Some(multipleRoutes.BasisForClaimController.show) else None,
      additionalDetailsChangeCallOpt =
        if !isPrintView then Some(multipleRoutes.EnterAdditionalDetailsController.show) else None
    )

  def renderForScheduled(claim: OverpaymentsScheduledJourney.Output, isPrintView: Boolean)(implicit
    messages: Messages
  ): SummaryList =
    render(
      journeyType = "scheduled",
      mrnRowsOpt = Some(
        makeMrnRowsForScheduled(
          claim.movementReferenceNumber,
          if !isPrintView then Some(scheduledRoutes.EnterMovementReferenceNumberController.show) else None
        )
      ),
      claimSummaryDocumentOpt = Some(claim.scheduledDocument),
      basisOfClaim = claim.basisOfClaim,
      newEoriAndDanOpt = claim.newEoriAndDan,
      additionalDetails = claim.additionalDetails,
      basisOfClaimChangeCallOpt = if !isPrintView then Some(multipleRoutes.BasisForClaimController.show) else None,
      additionalDetailsChangeCallOpt =
        if !isPrintView then Some(multipleRoutes.EnterAdditionalDetailsController.show) else None,
      claimSummaryDocumentChangeCallOpt =
        if !isPrintView then Some(scheduledRoutes.UploadMrnListController.show) else None
    )

}
