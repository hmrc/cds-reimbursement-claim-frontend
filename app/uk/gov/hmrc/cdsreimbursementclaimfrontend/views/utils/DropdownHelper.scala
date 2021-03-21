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

import javax.inject.Inject
import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.htmlUtil
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.components.paragraph_block
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.components.bullets

class DropdownHelper @Inject() (paragraph: paragraph_block, bulletList: bullets) extends htmlUtil {
  val landingKey: String = "landing"

  def landingDropDownContent()(implicit messages: Messages): Html =
    html(
      paragraph(Html(messages(s"$landingKey.details.p1")), Some("govuk-body")),
      bulletList(
        Seq(
          Html(messages(s"$landingKey.details.l1")),
          Html(messages(s"$landingKey.details.l2")),
          Html(messages(s"$landingKey.details.l3")),
          Html(messages(s"$landingKey.details.l4")),
          Html(messages(s"$landingKey.details.l5")),
          Html(messages(s"$landingKey.details.l6")),
          Html(messages(s"$landingKey.details.l7")),
          Html(messages(s"$landingKey.details.l8")),
          Html(messages(s"$landingKey.details.l9")),
          Html(messages(s"$landingKey.details.l10"))
        )
      )
    )

}
