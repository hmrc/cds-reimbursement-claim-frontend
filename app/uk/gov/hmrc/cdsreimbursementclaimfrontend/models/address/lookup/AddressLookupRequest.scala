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

import cats.implicits.catsSyntaxOptionId
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions.SelectPageConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions.TimeoutConfig

final case class AddressLookupRequest(
  version: Int,
  options: AddressLookupOptions
)

object AddressLookupRequest {

  def redirectBackTo(call: Call)(implicit viewConfig: ViewConfig, timeoutConfig: TimeoutConfig): Builder =
    Builder(
      AddressLookupOptions(
        continueUrl = viewConfig.buildCompleteSelfUrl(call),
        timeoutConfig = timeoutConfig
      )
    )

  final case class Builder(options: AddressLookupOptions) {

    def signOutUserVia(signOutUrl: String): Builder =
      copy(options.copy(signOutHref = signOutUrl.some))

    def searchUkAddressOnly(value: Boolean): Builder =
      copy(options.copy(ukMode = value.some))

    def maximumShow(maxToShow: Int): Builder =
      copy(options.copy(selectPageConfig = SelectPageConfig(proposalListLimit = maxToShow).some))

    def showBanner(value: Boolean): Builder =
      copy(options.copy(showPhaseBanner = value.some))

    def withServicePhaseLink(phase: String): Builder =
      copy(options.copy(phaseFeedbackLink = phase.some))

    def nameServiceAs(name: String): Builder =
      copy(options.copy(deskProServiceName = name.some))

    def makeAccessibilityFooterAvailableVia(link: String): Builder =
      copy(options.copy(accessibilityFooterUrl = link.some))

    def makePhaseFeedbackAvailableVia(link: String): Builder =
      copy(options.copy(phaseFeedbackLink = link.some))

    def showSearchAgainLink(value: Boolean): Builder =
      copy(options.copy(confirmPageConfig = options.confirmPageConfig.copy(showSearchAgainLink = value.some)))

    def showChangeLink(value: Boolean): Builder =
      copy(options.copy(confirmPageConfig = options.confirmPageConfig.copy(showChangeLink = value.some)))

    def showConfirmChangeText(value: Boolean): Builder =
      copy(options.copy(confirmPageConfig = options.confirmPageConfig.copy(showConfirmChangeText = value.some)))
  }

  implicit def builderToAddressLookupRequest(builder: Builder): AddressLookupRequest =
    AddressLookupRequest(version = 2, options = builder.options)

  implicit val format: OFormat[AddressLookupRequest] =
    Json.format[AddressLookupRequest]
}
