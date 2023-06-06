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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup

import play.api.libs.json.Json
import play.api.libs.json.OFormat

final case class AddressLookupLabels(
  en: Option[LanguageLabels] = None,
  cy: Option[LanguageLabels] = None
)
final case class LanguageLabels(
  lookupPageLabels: Option[PageLabels] = None,
  confirmPageLabels: Option[PageLabels] = None,
  selectPageLabels: Option[PageLabels] = None,
  editPageLabels: Option[PageLabels] = None,
  appLevelLabels: Option[AppLabels] = None
)

final case class AppLabels(navTitle: Option[String] = None, phaseBannerHtml: Option[String] = None)
final case class PageLabels(title: Option[String] = None, heading: Option[String] = None)

object AddressLookupLabels {
  implicit val appLabels: OFormat[AppLabels]              = Json.format[AppLabels]
  implicit val pageLabels: OFormat[PageLabels]            = Json.format[PageLabels]
  implicit val languageLabels: OFormat[LanguageLabels]    = Json.format[LanguageLabels]
  implicit val labelsConfig: OFormat[AddressLookupLabels] = Json.format[AddressLookupLabels]
}
