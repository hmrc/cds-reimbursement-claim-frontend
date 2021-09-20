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

import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HtmlUtil.html
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.components.{bullets, title_and_description}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils.BasisOfClaimsHints.{bulletList, titleAndDescription}

final case class BasisOfClaimsHints(items: Seq[Int]) extends AnyVal {

  def toHtml(parentKey: String)(implicit messages: Messages): Html =
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
}

object BasisOfClaimsHints {

  def skip(n: Int): BasisOfClaimsHints =
    BasisOfClaimsHints((0 to 13).drop(n))

  val titleAndDescription: title_and_description = new title_and_description()
  val bulletList: bullets                        = new bullets()
}
