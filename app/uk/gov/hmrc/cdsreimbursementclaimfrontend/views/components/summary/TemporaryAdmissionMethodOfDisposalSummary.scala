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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import play.api.data.Form
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal._
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem

object TemporaryAdmissionMethodOfDisposalSummary {

  def apply(implicit
    messages: Messages,
    form: Form[Option[TemporaryAdmissionMethodOfDisposal]]
  ): Seq[RadioItem] =
    List(
      exportMethodItem(ExportedInSingleShipment),
      exportMethodItem(ExportedInMultipleShipments),
      exportMethodItem(DeclaredToOtherTraderUnderTemporaryAdmission),
      exportMethodItem(DeclaredToFreeCirculation),
      exportMethodItem(DeclaredToInwardProcessingRelief),
      exportMethodItem(DeclaredToEndUse),
      exportMethodItem(DeclaredToAFreeZone),
      exportMethodItem(DeclaredToACustomsWarehouse),
      exportMethodItem(Destroyed),
      exportMethodItem(Other),
      RadioItem(divider = Some("or")),
      exportMethodItem(MultipleDisposalMethodsWereUsed)
    )

  private def exportMethodItem(
    exportMethod: TemporaryAdmissionMethodOfDisposal
  )(implicit form: Form[Option[TemporaryAdmissionMethodOfDisposal]], messages: Messages): RadioItem =
    RadioItem(
      value = Some(TemporaryAdmissionMethodOfDisposal.keyOf(exportMethod)),
      content = Text(
        messages(
          s"choose-export-method.export-method-description.${TemporaryAdmissionMethodOfDisposal.keyOf(exportMethod)}"
        )
      ),
      checked = form.value.flatten.contains(exportMethod)
    )
}
