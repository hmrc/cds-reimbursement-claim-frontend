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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ScheduledDocumentAnswer
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

import javax.inject.{Inject, Singleton}

@Singleton
class ScheduleDocumentHelper @Inject() (implicit langs: Langs, messages: MessagesApi) {

  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "schedule-document.review"

  def makeUploadedDocumentRow(uploadedDocument: ScheduledDocumentAnswer): List[SummaryListRow] = {
    val fileName         = uploadedDocument.uploadDocument.fileName
    val scheduleDocument =
      SummaryListRow(
        key = Key(Text(messages(s"$key.file-label")(lang))),
        value = Value(Text(fileName)),
        actions = Some(
          Actions(
            items = Seq(
              ActionItem(
                href = s"${fileUploadRoutes.ScheduleOfMrnDocumentController.deleteScheduledDocument().url}",
                content = Text(messages(s"$key.remove")(lang))
              )
            )
          )
        )
      )

    List(scheduleDocument)
  }
}
