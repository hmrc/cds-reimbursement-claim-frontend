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

import cats.implicits.catsSyntaxOptionId
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions.SelectPageConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions.TimeoutConfig

final case class AddressLookupRequest(
  version: Int,
  options: AddressLookupOptions,
  labels: AddressLookupLabels
)

object AddressLookupRequest {

  def redirectBackTo(continueUrl: String)(implicit timeoutConfig: TimeoutConfig): Builder =
    Builder(
      AddressLookupOptions(
        continueUrl = continueUrl,
        timeoutConfig = timeoutConfig
      ),
      AddressLookupLabels()
    )

  final case class Builder(options: AddressLookupOptions, labels: AddressLookupLabels) {

    def signOutUserVia(signOutUrl: String): Builder =
      copy(options.copy(signOutHref = signOutUrl.some))

    def whetherSearchOnlyUkAddresses(value: Boolean): Builder =
      copy(options.copy(ukMode = value.some))

    def showMax(maxToShow: Int): Builder =
      copy(options.copy(selectPageConfig = SelectPageConfig(proposalListLimit = maxToShow).some))

    def whetherShowBanner(value: Boolean): Builder =
      copy(options.copy(showPhaseBanner = value.some))

    def withServicePhaseLink(phase: String): Builder =
      copy(options.copy(phaseFeedbackLink = phase.some))

    def nameConsumerServiceAs(name: String): Builder =
      copy(options.copy(deskProServiceName = name.some))

    def makeAccessibilityFooterAvailableVia(link: String): Builder =
      copy(options.copy(accessibilityFooterUrl = link.some))

    def makePhaseFeedbackAvailableVia(link: String): Builder =
      copy(options.copy(phaseFeedbackLink = link.some))

    def whetherShowSearchAgainLink(value: Boolean): Builder =
      copy(options.copy(confirmPageConfig = options.confirmPageConfig.copy(showSearchAgainLink = value.some)))

    def whetherShowChangeLink(value: Boolean): Builder =
      copy(options.copy(confirmPageConfig = options.confirmPageConfig.copy(showChangeLink = value.some)))

    def whetherShowConfirmChangeText(value: Boolean): Builder =
      copy(options.copy(confirmPageConfig = options.confirmPageConfig.copy(showConfirmChangeText = value.some)))

    def disableTranslations(value: Boolean): Builder =
      copy(options.copy(disableTranslations = Some(value)))

    def withPageTitles(
      appTitle: Option[String] = None,
      phaseBannerHtml: Option[String] = None,
      lookupTitle: Option[String],
      confirmTitle: Option[String],
      selectTitle: Option[String],
      editTitle: Option[String],
      lookupHeading: Option[String],
      confirmHeading: Option[String],
      selectHeading: Option[String],
      editHeading: Option[String]
    ): Builder =
      copy(labels =
        labels.copy(en =
          LanguageLabels(
            appLevelLabels = AppLabels(appTitle, phaseBannerHtml).some,
            lookupPageLabels = PageLabels(lookupTitle, lookupHeading).some,
            confirmPageLabels = PageLabels(confirmTitle, confirmHeading).some,
            selectPageLabels = PageLabels(selectTitle, selectHeading).some,
            editPageLabels = PageLabels(editTitle, editHeading).some
          ).some
        )
      )
  }

  implicit def builderToAddressLookupRequest(builder: Builder): AddressLookupRequest =
    AddressLookupRequest(version = 2, options = builder.options, labels = builder.labels)

  implicit val format: OFormat[AddressLookupRequest] =
    Json.format[AddressLookupRequest]
}
