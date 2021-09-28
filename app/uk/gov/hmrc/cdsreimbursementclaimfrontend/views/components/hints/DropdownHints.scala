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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints

import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HtmlUtil.html
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints.{bulletList, titleAndDescription}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.components.{bullets, title_and_description}

final case class DropdownHints(items: Seq[Int]) extends Hints {

  def renderHintsWithLabels(parentKey: String)(implicit messages: Messages): Html =
    html(
      bulletList(
        items.map(i =>
          titleAndDescription(
            messages(s"$parentKey.details.b$i"),
            messages(s"$parentKey.details.l$i")
          )
        )
      )
    )

  def renderHints(parentKey: String)(implicit messages: Messages): Html = {
    val listOfMessages: Seq[Html] = for (i <- items) yield Html(messages(s"$parentKey.details.l$i"))
    html(
      bulletList(
        listOfMessages
      )
    )
  }
}

object DropdownHints {

  def range(elementIndex: Int, maxHints: Int): DropdownHints =
    DropdownHints(elementIndex to maxHints)

  val titleAndDescription: title_and_description = new title_and_description()
  val bulletList: bullets                        = new bullets()
}
