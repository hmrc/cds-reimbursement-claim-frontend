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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.Options._

final case class Options(
  continueUrl: String,
  signOutHref: Option[String] = None,
  accessibilityFooterUrl: Option[String] = None,
  selectPageConfig: Option[SelectPageConfig] = None,
  timeoutConfig: TimeoutConfig,
  confirmPageConfig: Option[ConfirmPageConfig] = Some(ConfirmPageConfig()),
  phaseFeedbackLink: Option[String] = Some("private-beta"),
  deskProServiceName: Option[String] = Some("cds-reimbursement-claim"),
  showPhaseBanner: Option[Boolean] = Some(true),
  ukMode: Option[Boolean] = Some(true)
)

object Options {

  final case class SelectPageConfig(proposalListLimit: Option[Int])

  final case class ConfirmPageConfig(
    showChangeLink: Option[Boolean] = Some(true),
    showSearchAgainLink: Option[Boolean] = Some(true),
    showConfirmChangeText: Option[Boolean] = Some(true)
  )

  final case class TimeoutConfig(
    timeoutAmount: Long,
    timeoutUrl: String,
    timeoutKeepAliveUrl: Option[String] = None
  )

  implicit val selectPageConfigFormat: OFormat[SelectPageConfig]   = Json.format[SelectPageConfig]
  implicit val confirmPageConfigFormat: OFormat[ConfirmPageConfig] = Json.format[ConfirmPageConfig]
  implicit val timeoutConfigFormat: OFormat[TimeoutConfig]         = Json.format[TimeoutConfig]
  implicit val addressLookupOptionsFormat: OFormat[Options]        = Json.format[Options]
}
