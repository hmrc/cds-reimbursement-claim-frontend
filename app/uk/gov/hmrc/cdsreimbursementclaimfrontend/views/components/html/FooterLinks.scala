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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.govukfrontend.views.viewmodels.footer.FooterItem

object FooterLinks {

  def apply()(implicit messages: Messages, viewConfig: ViewConfig): Seq[FooterItem] =
    viewConfig.footerLinkItems.flatMap { item =>
      val keyPrefix = s"footer.$item"
      val textKey   = s"$keyPrefix.text"
      val urlKey    = s"$keyPrefix.url"

      if messages.isDefinedAt(textKey) && messages.isDefinedAt(urlKey) then
        Seq(
          FooterItem(
            text = Some(messages(textKey)),
            href = Some(messages(urlKey))
          )
        )
      else Seq.empty
    }
}
