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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupLabels
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AppLabels
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.ConfirmPageLabels
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.CountryPickerLabels
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.EditPageLabels
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.LanguageLabels
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.LookupPageLabels
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.PageLabels
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.SelectPageLabels
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupLabels.*

class AddressLookupLabelsSpec extends AnyWordSpec with Matchers {

  "PageLabels" should {
    "serialize and deserialize" in {

      val labels = PageLabels(
        title = Some("A Title"),
        heading = Some("A Heading")
      )

      val json = Json.toJson(labels)
      json shouldBe Json.obj(
        "title"   -> "A Title",
        "heading" -> "A Heading"
      )

      val deserialized = json.as[PageLabels]
      deserialized shouldBe labels
    }
  }

  "AppLabels" should {
    "serialize and deserialize" in {

      val labels = AppLabels(
        navTitle = Some("A Title"),
        phaseBannerHtml = Some("A Banner")
      )

      val json = Json.toJson(labels)
      json shouldBe Json.obj(
        "navTitle"        -> "A Title",
        "phaseBannerHtml" -> "A Banner"
      )

      val deserialized = json.as[AppLabels]
      deserialized shouldBe labels
    }
  }

  "SelectPageLabels" should {
    "serialize and deserialize" in {

      val labels = SelectPageLabels(
        title = Some("Some title"),
        heading = Some("Some heading"),
        headingWithPostcode = Some("Some postcode"),
        proposalListLabel = Some("Some selection"),
        submitLabel = Some("Some submit"),
        searchAgainLinkText = Some("Some search again"),
        editAddressLinkText = Some("Some address")
      )

      val json = Json.toJson(labels)
      json shouldBe Json.obj(
        "title"               -> "Some title",
        "heading"             -> "Some heading",
        "headingWithPostcode" -> "Some postcode",
        "proposalListLabel"   -> "Some selection",
        "submitLabel"         -> "Some submit",
        "searchAgainLinkText" -> "Some search again",
        "editAddressLinkText" -> "Some address"
      )

      val deserialized = json.as[SelectPageLabels]
      deserialized shouldBe labels
    }
  }

  "EditPageLabels" should {
    "serialize and deserialize" in {

      val labels = EditPageLabels(
        title = Some("Some title"),
        heading = Some("Some heading"),
        organisationLabel = Some("Some organisation"),
        line1Label = Some("Some address"),
        line2Label = Some("Some address line 2"),
        line3Label = Some("Some address line 3"),
        townLabel = Some("Some Town"),
        postcodeLabel = Some("Some postcode"),
        countryLabel = Some("Some country"),
        submitLabel = Some("Some submit")
      )

      val json = Json.toJson(labels)
      json shouldBe Json.obj(
        "title"             -> "Some title",
        "heading"           -> "Some heading",
        "organisationLabel" -> "Some organisation",
        "line1Label"        -> "Some address",
        "line2Label"        -> "Some address line 2",
        "line3Label"        -> "Some address line 3",
        "townLabel"         -> "Some Town",
        "postcodeLabel"     -> "Some postcode",
        "countryLabel"      -> "Some country",
        "submitLabel"       -> "Some submit"
      )

      val deserialized = json.as[EditPageLabels]
      deserialized shouldBe labels
    }
  }

  "LookupPageLabels" should {
    "serialize and deserialize" in {

      val labels = LookupPageLabels(
        title = Some("Some title"),
        heading = Some("Some heading"),
        afterHeadingText = Some("Some text"),
        filterLabel = Some("Some filter"),
        submitLabel = Some("Some submit"),
        postcodeLabel = Some("Some postcode"),
        noResultsFoundMessage = Some("Some message"),
        resultLimitExceededMessage = Some("Some result"),
        manualAddressLinkText = Some("Some entry")
      )

      val json = Json.toJson(labels)
      json shouldBe Json.obj(
        "title"                      -> "Some title",
        "heading"                    -> "Some heading",
        "afterHeadingText"           -> "Some text",
        "filterLabel"                -> "Some filter",
        "submitLabel"                -> "Some submit",
        "postcodeLabel"              -> "Some postcode",
        "noResultsFoundMessage"      -> "Some message",
        "resultLimitExceededMessage" -> "Some result",
        "manualAddressLinkText"      -> "Some entry"
      )

      val deserialized = json.as[LookupPageLabels]
      deserialized shouldBe labels
    }
  }

  "LanguageLabels" should {
    "serialize and deserialize" in {

      val labels = LanguageLabels(
        countryPickerLabels = Some(CountryPickerLabels(title = Some("Some country"))),
        lookupPageLabels = Some(LookupPageLabels(title = Some("Some address"))),
        confirmPageLabels = Some(ConfirmPageLabels(title = Some("Some confirmation"))),
        selectPageLabels = Some(SelectPageLabels(title = Some("Some selection"))),
        editPageLabels = Some(EditPageLabels(title = Some("Some edit"))),
        appLevelLabels = Some(AppLabels(navTitle = Some("Some navigation")))
      )

      val json         = Json.toJson(labels)
      val deserialized = json.as[LanguageLabels]
      deserialized shouldBe labels
    }
  }

  "AddressLookupLabels" should {
    "serialize and deserialize" in {

      val labels = AddressLookupLabels(
        en = Some(
          LanguageLabels(
            appLevelLabels = Some(AppLabels(navTitle = Some("Some English Navigation")))
          )
        ),
        cy = Some(
          LanguageLabels(
            appLevelLabels = Some(AppLabels(navTitle = Some("Some Welsh Navigation")))
          )
        )
      )

      val json         = Json.toJson(labels)
      val deserialized = json.as[AddressLookupLabels]
      deserialized shouldBe labels
    }
  }

  "CountryPickerLabels" should {
    "serialize and deserialize" in {

      val labels = CountryPickerLabels(
        title = Some("Some title"),
        heading = Some("Some heading"),
        countryLabel = Some("Some label"),
        submitLabel = Some("Some submit required")
      )

      val json = Json.toJson(labels)
      json shouldBe Json.obj(
        "title"        -> "Some title",
        "heading"      -> "Some heading",
        "countryLabel" -> "Some label",
        "submitLabel"  -> "Some submit required"
      )

      val deserialized = json.as[CountryPickerLabels]
      deserialized shouldBe labels
    }
  }

  "ConfirmPageLabels" should {
    "serialize and deserialize" in {

      val labels = ConfirmPageLabels(
        title = Some("Some address"),
        heading = Some("Some heading"),
        infoSubheading = Some("Some subheading"),
        infoMessage = Some("Some information"),
        submitLabel = Some("Some submit required"),
        searchAgainLinkText = Some("Some search"),
        changeLinkText = Some("Some change required"),
        confirmChangeText = Some("Some confirmation on change required")
      )

      val json = Json.toJson(labels)
      json shouldBe Json.obj(
        "title"               -> "Some address",
        "heading"             -> "Some heading",
        "infoSubheading"      -> "Some subheading",
        "infoMessage"         -> "Some information",
        "submitLabel"         -> "Some submit required",
        "searchAgainLinkText" -> "Some search",
        "changeLinkText"      -> "Some change required",
        "confirmChangeText"   -> "Some confirmation on change required"
      )

      val deserialized = json.as[ConfirmPageLabels]
      deserialized shouldBe labels
    }
  }
}
