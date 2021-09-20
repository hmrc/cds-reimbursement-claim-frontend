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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.whoIsMakingTheClaimKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HtmlUtil
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.components.bullets
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.components.title_and_description

class DropDownHelper @Inject() (
  bulletList: bullets,
  titleAndDescription: title_and_description
) extends HtmlUtil {

  def render(typesOfEvidences: Seq[UploadDocumentType], parentKey: String)(implicit messages: Messages): Html =
    html(
      bulletList(
        typesOfEvidences.map { evidenceType =>
          titleAndDescription(
            messages(s"$parentKey.details.b${evidenceType.index}"),
            messages(s"$parentKey.details.l${evidenceType.index}")
          )
        }
      )
    )

  def whoIsDropDownContent()(implicit messages: Messages): Html =
    html(
      bulletList(
        Seq(
          Html(messages(s"$whoIsMakingTheClaimKey.details.l0")),
          Html(messages(s"$whoIsMakingTheClaimKey.details.l1")),
          Html(messages(s"$whoIsMakingTheClaimKey.details.l2"))
        )
      )
    )

}
