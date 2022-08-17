/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.implicits.catsSyntaxEq
import play.api.data.Form
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem

object TemporaryAdmissionMethodOfDisposalSummary {
  def apply(
    exportMethods: Seq[TemporaryAdmissionMethodOfDisposal],
    messages: Messages,
    form: Form[Option[TemporaryAdmissionMethodOfDisposal]]
  ): Seq[RadioItem] =
    exportMethods.zipWithIndex.flatMap {
      case (em, index) if index === exportMethods.length - 1 =>
        List(
          RadioItem(
            divider = Some("or")
          ),
          RadioItem(
            value = Some(TemporaryAdmissionMethodOfDisposal.keyOf(em)),
            content = Text(
              messages(
                s"choose-export-method.export-method-description.${TemporaryAdmissionMethodOfDisposal.keyOf(em)}"
              )
            ),
            checked = form.value.contains(em)
          )
        )
      case (em, _)                                           =>
        List(
          RadioItem(
            value = Some(TemporaryAdmissionMethodOfDisposal.keyOf(em)),
            content = Text(
              messages(
                s"choose-export-method.export-method-description.${TemporaryAdmissionMethodOfDisposal.keyOf(em)}"
              )
            ),
            checked = form.value.contains(em)
          )
        )
    }
}
