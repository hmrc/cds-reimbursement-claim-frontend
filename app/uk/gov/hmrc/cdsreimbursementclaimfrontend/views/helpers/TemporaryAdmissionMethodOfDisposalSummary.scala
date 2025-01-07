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

import play.api.data.Form
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal._
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.checkboxes.CheckboxItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.hint.Hint

object TemporaryAdmissionMethodOfDisposalSummary {

  def apply(implicit
    messages: Messages,
    form: Form[List[TemporaryAdmissionMethodOfDisposal]]
  ): Seq[CheckboxItem] =
    List(
      exportMethodItem(ExportedInSingleOrMultipleShipments),
      exportMethodItem(DeclaredToACustomsWarehouse, hasHintText = true),
      exportMethodItem(DeclaredToAFreeZone, hasHintText = true),
      exportMethodItem(DeclaredToEndUse, hasHintText = true),
      exportMethodItem(DeclaredToFreeCirculation, hasHintText = true),
      exportMethodItem(DeclaredToInwardProcessingRelief, hasHintText = true),
      exportMethodItem(DeclaredToOtherTraderUnderTemporaryAdmission),
      exportMethodItem(Destroyed),
      exportMethodItem(Other)
    )

  private def exportMethodItem(
    exportMethod: TemporaryAdmissionMethodOfDisposal,
    hasHintText: Boolean = false
  )(implicit form: Form[List[TemporaryAdmissionMethodOfDisposal]], messages: Messages): CheckboxItem =
    CheckboxItem(
      value = TemporaryAdmissionMethodOfDisposal.keyOf(exportMethod),
      content = Text(
        messages(
          s"choose-export-method.export-method-description.${TemporaryAdmissionMethodOfDisposal.keyOf(exportMethod)}"
        )
      ),
      hint =
        if (hasHintText)
          Some(
            Hint(content =
              Text(
                messages(
                  s"choose-export-method.export-method-hint.${TemporaryAdmissionMethodOfDisposal.keyOf(exportMethod)}"
                )
              )
            )
          )
        else None,
      checked = form.value.exists(mods => mods.contains(exportMethod))
    )
}
