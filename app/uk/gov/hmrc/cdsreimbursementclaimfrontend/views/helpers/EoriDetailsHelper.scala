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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers

import cats.implicits._
import play.api.i18n.Lang
import play.api.i18n.Langs
import play.api.i18n.MessagesApi
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Key
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.Value

import javax.inject.Inject
import javax.inject.Singleton

@Singleton
class EoriDetailsHelper @Inject() (implicit langs: Langs, messages: MessagesApi) {

  val lang: Lang = langs.availables.headOption.getOrElse(Lang.defaultLang)

  private val key = "check-eori-details"

  def eoriSummary(user: SignedInUserDetails): List[SummaryListRow] =
    List(
      makeRow("eori-number", user.eori.value),
      makeRow("name", user.contactName.value)
    ).flattenOption

  private def makeRow(fieldName: String, fieldValue: String): Option[SummaryListRow] =
    Some(SummaryListRow(Key(content = HtmlContent(messages(s"$key.$fieldName.label")(lang))), Value(Text(fieldValue))))

}
