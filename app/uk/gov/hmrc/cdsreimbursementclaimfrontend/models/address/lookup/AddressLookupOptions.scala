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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions._

final case class AddressLookupOptions(
  continueUrl: String,
  timeoutConfig: TimeoutConfig,
  signOutHref: Option[String] = None,
  accessibilityFooterUrl: Option[String] = None,
  selectPageConfig: Option[SelectPageConfig] = None,
  confirmPageConfig: ConfirmPageConfig = ConfirmPageConfig(),
  phaseFeedbackLink: Option[String] = None,
  deskProServiceName: Option[String] = None,
  showPhaseBanner: Option[Boolean] = None,
  ukMode: Option[Boolean] = None,
  labels: Labels = Labels()
)

object AddressLookupOptions {

  final case class SelectPageConfig(proposalListLimit: Int)

  final case class ConfirmPageConfig(
    showChangeLink: Option[Boolean] = None,
    showSearchAgainLink: Option[Boolean] = None,
    showConfirmChangeText: Option[Boolean] = None
  )

  final case class TimeoutConfig(
    timeoutAmount: Int,
    timeoutUrl: String,
    timeoutKeepAliveUrl: Option[String] = None
  )

  final case class Labels(
    lookupPageLabels: Option[PageLabels] = None,
    confirmPageLabels: Option[PageLabels] = None,
    selectPageLabels: Option[PageLabels] = None,
    editPageLabels: Option[PageLabels] = None
  )
  final case class PageLabels(title: String)

  implicit val selectPageConfigFormat: OFormat[SelectPageConfig]         = Json.format[SelectPageConfig]
  implicit val confirmPageConfigFormat: OFormat[ConfirmPageConfig]       = Json.format[ConfirmPageConfig]
  implicit val timeoutConfigFormat: OFormat[TimeoutConfig]               = Json.format[TimeoutConfig]
  implicit val addressLookupOptionsFormat: OFormat[AddressLookupOptions] = Json.format[AddressLookupOptions]
  implicit val labelsConfig: OFormat[Labels]                             = Json.format[Labels]
  implicit val pageLabels: OFormat[PageLabels]                           = Json.format[PageLabels]
}
