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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HtmlUtil
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.components.bullets
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.components.title_and_description

class DropDownHelper @Inject() (
  bulletList: bullets,
  titleAndDescription: title_and_description
) extends HtmlUtil {

  val reasonAndBasisBasisKey  = "select-reason-and-basis-for-claim.basis.details"
  val reasonAndBasisReasonKey = "select-reason-and-basis-for-claim.reason.details"
  val whoIsMakingTheClaimKey  = "select-who-is-making-the-claim.details"

  def render(typesOfEvidences: Seq[UploadDocumentType], parentKey: String)(implicit messages: Messages): Html =
    html(
      bulletList(
        typesOfEvidences.map { evidenceType =>
          Html(messages(s"$parentKey.details.l${evidenceType.index}"))
        }
      )
    )

  def reasonAndBasisBasisDropDownContent()(implicit messages: Messages): Html =
    html(
      bulletList(
        Seq(
          Html(messages(s"$reasonAndBasisBasisKey.l0")),
          Html(messages(s"$reasonAndBasisBasisKey.l1")),
          Html(messages(s"$reasonAndBasisBasisKey.l2")),
          Html(messages(s"$reasonAndBasisBasisKey.l3")),
          Html(messages(s"$reasonAndBasisBasisKey.l4")),
          Html(messages(s"$reasonAndBasisBasisKey.l5")),
          Html(messages(s"$reasonAndBasisBasisKey.l6")),
          Html(messages(s"$reasonAndBasisBasisKey.l7")),
          Html(messages(s"$reasonAndBasisBasisKey.l8")),
          Html(messages(s"$reasonAndBasisBasisKey.l9"))
        )
      )
    )

  def reasonAndBasisReasonDropDownContent()(implicit messages: Messages): Html =
    html(
      bulletList(
        Seq(
          Html(messages(s"$reasonAndBasisReasonKey.l0")),
          Html(messages(s"$reasonAndBasisReasonKey.l1")),
          Html(messages(s"$reasonAndBasisReasonKey.l2"))
        )
      )
    )

  def render(basisOfClaimsExamples: BasisOfClaimsHints, parentKey: String)(implicit messages: Messages): Html =
    html(
      bulletList(
        basisOfClaimsExamples.items.map(i =>
          titleAndDescription(
            messages(basisOfClaimsExamples.buildLabelKey(parentKey, i)),
            messages(basisOfClaimsExamples.buildTextKey(parentKey, i))
          )
        )
      )
    )

  def whoIsDropDownContent()(implicit messages: Messages): Html =
    html(
      bulletList(
        Seq(
          Html(messages(s"$whoIsMakingTheClaimKey.l0")),
          Html(messages(s"$whoIsMakingTheClaimKey.l1")),
          Html(messages(s"$whoIsMakingTheClaimKey.l2"))
        )
      )
    )

}
