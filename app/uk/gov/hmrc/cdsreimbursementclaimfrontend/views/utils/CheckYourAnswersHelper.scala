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

import cats.implicits._
import play.api.i18n.{Lang, Langs, MessagesApi}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CompleteClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist._

import javax.inject.{Inject, Singleton}

@Singleton
class CheckYourAnswersHelper @Inject() (implicit langs: Langs, messages: MessagesApi) {

  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "check-your-answers"

  def cyaSummary(completeClaim: CompleteClaim): List[SummaryListRow] =
    List(
      makeReferenceNumberRow(completeClaim.movementReferenceNumber)
    ).flattenOption

  private def makeReferenceNumberRow(number: Either[EntryNumber, MRN]): Option[SummaryListRow] =
    number match {
      case Left(value)  =>
        Some(
          SummaryListRow(
            key = Key(Text(messages(s"$key.entry-reference-number.label")(lang))),
            value = Value(Text(value.value)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = s"${routes.EnterMovementReferenceNumberController.changeMrn.url}",
                    content = Text("cya.change")
                  )
                )
              )
            )
          )
        )
      case Right(value) =>
        Some(
          SummaryListRow(
            key = Key(Text(messages(s"$key.mrn.label")(lang))),
            value = Value(Text(value.value)),
            actions = Some(
              Actions(
                items = Seq(
                  ActionItem(
                    href = s"${routes.EnterMovementReferenceNumberController.changeMrn.url}",
                    content = Text("cya.change")
                  )
                )
              )
            )
          )
        )
    }

//  private def makeSupportingEvidenceRow(
//    completeSupportingEvidenceAnswer: CompleteSupportingEvidenceAnswer
//  ): SummaryList = {
//    val fileUploadSummaryRows = completeSupportingEvidenceAnswer.evidences.map { evidence =>
//      SummaryListRow(
//        key = Key(Text(messages(s"$key.mrn.label")(lang))),
//        value = Value(Text(evidence.fileName)),
//        actions = Some(
//          Actions(
//            items = Seq(
//              ActionItem(
//                href = s"${fileUploadRoutes.SupportingEvidenceController.checkYourAnswers().url}",
//                content = Text("Change")
//              )
//            )
//          )
//        )
//      )
//    }
//    SummaryList(rows = fileUploadSummaryRows)
//  }

}
