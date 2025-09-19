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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*

object CheckYourAnswersTemporaryAdmissionMethodOfDisposalSummary {

  def apply(
    claim: SecuritiesJourney.Output,
    mods: Seq[TemporaryAdmissionMethodOfDisposal],
    isPrintView: Boolean
  )(implicit
    messages: Messages
  ): SummaryList =
    SummaryList(
      Seq(
        SummaryListRow(
          key = Key(HtmlContent(messages(s"check-your-answers.securities.claim-details.method-of-disposal"))),
          value = Value(
            HtmlContent(
              mods.map(mod => messages(s"choose-export-method.export-method-description.$mod")).mkString("<br/>")
            )
          ),
          actions =
            if (isPrintView) None
            else
              Some(
                Actions(
                  items = Seq(
                    ActionItem(
                      href = routes.ChooseExportMethodController.show.url,
                      content = Text(messages("cya.change")),
                      visuallyHiddenText =
                        Some(messages(s"check-your-answers.securities.claim-details.method-of-disposal.hidden"))
                    )
                  )
                )
              )
        )
      ) ++
        claim.exportMovementReferenceNumber
          .map { exportDeclarationMrns =>
            exportDeclarationMrns.zipWithIndex.map { case (exportDeclarationMrn, mrnIndex) =>
              SummaryListRow(
                key = Key(HtmlContent(OrdinalNumberExportMrnHelper(mrnIndex + 1))),
                value = Value(Text(exportDeclarationMrn.value)),
                actions =
                  if (isPrintView) None
                  else
                    Some(
                      Actions(
                        items = Seq(
                          ActionItem(
                            href = routes.EnterExportMovementReferenceNumberController.showNext(mrnIndex + 1).url,
                            content = Text(messages("cya.change")),
                            visuallyHiddenText = Some(OrdinalNumberExportMrnHelper(mrnIndex + 1))
                          )
                        )
                      )
                    )
              )
            }
          }
          .getOrElse(Seq.empty)
    )
}
