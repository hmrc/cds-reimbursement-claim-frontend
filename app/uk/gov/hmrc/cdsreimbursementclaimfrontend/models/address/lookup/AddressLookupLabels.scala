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
  countryPickerLabels: Option[CountryPickerLabels] = None,
  lookupPageLabels: Option[LookupPageLabels] = None,
  confirmPageLabels: Option[ConfirmPageLabels] = None,
  selectPageLabels: Option[SelectPageLabels] = None,
  editPageLabels: Option[EditPageLabels] = None,
  appLevelLabels: Option[AppLabels] = None
)

final case class AppLabels(navTitle: Option[String] = None, phaseBannerHtml: Option[String] = None)
final case class PageLabels(title: Option[String] = None, heading: Option[String] = None)

object AddressLookupLabels {
  implicit val appLabels: OFormat[AppLabels]                     = Json.format[AppLabels]
  implicit val pageLabels: OFormat[PageLabels]                   = Json.format[PageLabels]
  implicit val countryPickerLabels: OFormat[CountryPickerLabels] = Json.format[CountryPickerLabels]
  implicit val selectPageLabels: OFormat[SelectPageLabels]       = Json.format[SelectPageLabels]
  implicit val confirmPageLabels: OFormat[ConfirmPageLabels]     = Json.format[ConfirmPageLabels]
  implicit val editPageLabels: OFormat[EditPageLabels]           = Json.format[EditPageLabels]
  implicit val lookupPageLabels: OFormat[LookupPageLabels]       = Json.format[LookupPageLabels]
  implicit val languageLabels: OFormat[LanguageLabels]           = Json.format[LanguageLabels]
  implicit val labelsConfig: OFormat[AddressLookupLabels]        = Json.format[AddressLookupLabels]
}

// "countryPickerLabels": {
//   "title": "Custom title",
//   "heading": "Custom heading",
//   "countryLabel": "Custom country label",
//   "submitLabel": "Custom submit label"
// }
final case class CountryPickerLabels(
  title: Option[String] = None,
  heading: Option[String] = None,
  countryLabel: Option[String] = None,
  submitLabel: Option[String] = None
)

// "selectPageLabels": {
//   "title": "Choose address",
//   "heading": "Choose address",
//   "headingWithPostcode": "foo",
//   "proposalListLabel": "Please select one of the following addresses",
//   "submitLabel": "Continue",
//   "searchAgainLinkText": "Search again",
//   "editAddressLinkText": "Enter address manually"
// }
final case class SelectPageLabels(
  title: Option[String] = None,
  heading: Option[String] = None,
  headingWithPostcode: Option[String] = None,
  proposalListLabel: Option[String] = None,
  submitLabel: Option[String] = None,
  searchAgainLinkText: Option[String] = None,
  editAddressLinkText: Option[String] = None
)

// "confirmPageLabels": {
//   "title": "Confirm address",
//   "heading": "Review and confirm",
//   "infoSubheading": "Your selected address",
//   "infoMessage": "This is how your address will look. Please double-check it and, if accurate, click on the <kbd>Confirm</kbd> button.",
//   "submitLabel": "Confirm Address",
//   "searchAgainLinkText": "Search again",
//   "changeLinkText": "Edit address",
//   "confirmChangeText": "By confirming this change, you agree that the information you have given is complete and correct."
// }
final case class ConfirmPageLabels(
  title: Option[String] = None,
  heading: Option[String] = None,
  infoSubheading: Option[String] = None,
  infoMessage: Option[String] = None,
  submitLabel: Option[String] = None,
  searchAgainLinkText: Option[String] = None,
  changeLinkText: Option[String] = None,
  confirmChangeText: Option[String] = None
)

// "editPageLabels": {
//   "title": "Enter address",
//   "heading": "Enter address",
//   "organisationLabel": "Organisation (optional)",
//   "line1Label": "Address line 1",
//   "line2Label": "Address line 2 (optional)",
//   "line3Label": "Address line 3 (optional)",
//   "townLabel": "Town/City",
//   "postcodeLabel": "Postcode (optional)",
//   "countryLabel": "Country",
//   "submitLabel": "Continue"
// }
final case class EditPageLabels(
  title: Option[String] = None,
  heading: Option[String] = None,
  organisationLabel: Option[String] = None,
  line1Label: Option[String] = None,
  line2Label: Option[String] = None,
  line3Label: Option[String] = None,
  townLabel: Option[String] = None,
  postcodeLabel: Option[String] = None,
  countryLabel: Option[String] = None,
  submitLabel: Option[String] = None
)

// "lookupPageLabels": {
//   "title": "Find address",
//   "heading": "Find address",
//   "afterHeadingText": "We will use this address to send letters",
//   "filterLabel": "Property name or number (optional)",
//   "postcodeLabel": "Postcode",
//   "submitLabel": "Find address",
//   "noResultsFoundMessage": "Sorry, we couldn't find anything for that postcode.",
//   "resultLimitExceededMessage": "There were too many results. Please add additional details to limit the number of results.",
//   "manualAddressLinkText": "Enter the address manually"
// }
final case class LookupPageLabels(
  title: Option[String] = None,
  heading: Option[String] = None,
  afterHeadingText: Option[String] = None,
  filterLabel: Option[String] = None,
  submitLabel: Option[String] = None,
  postcodeLabel: Option[String] = None,
  noResultsFoundMessage: Option[String] = None,
  resultLimitExceededMessage: Option[String] = None,
  manualAddressLinkText: Option[String] = None
)
