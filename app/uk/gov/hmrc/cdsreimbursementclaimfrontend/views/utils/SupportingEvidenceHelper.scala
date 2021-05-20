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

import play.api.i18n.{Lang, Langs, MessagesApi}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidence
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

import javax.inject.{Inject, Singleton}

@Singleton
class SupportingEvidenceHelper @Inject() (implicit langs: Langs, messages: MessagesApi) {

  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "supporting-evidence.check-your-answers"

  def makeUploadedFilesRows(supportingEvidences: List[SupportingEvidence], isAmend: Boolean): List[SummaryListRow] =
    supportingEvidences.zipWithIndex.map { case (document, fileIndex) =>
      SummaryListRow(
        key = Key(Text(messages(s"$key.file-label", fileIndex + 1)(lang))),
        value = Value(Text(document.fileName)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href =
                  s"${fileUploadRoutes.SupportingEvidenceController.deleteSupportingEvidence(document.uploadReference, isAmend, addNew = false).url}",
                content = Text(messages("cya.remove")(lang))
              )
            )
          )
        )
      )
    }

}
