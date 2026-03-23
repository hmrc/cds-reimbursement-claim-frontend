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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{ContactAddress, Country}

class ContactAddressSpec extends AnyWordSpec with Matchers {

  "ContactAddress" should {

    "overflow excess characters" in {
      ContactAddress("", Some(""), None, "Foo", "Foo", Country.uk)
        .overflowExcessCharacters()
        .shouldBe(ContactAddress("", Some(""), None, "Foo", "Foo", Country.uk))

      ContactAddress(" " * 100, Some(" " * 100), None, "Foo", "Foo", Country.uk)
        .overflowExcessCharacters()
        .shouldBe(ContactAddress(" " * 34, Some(" " * 34), Some(" " * 30 + ",   "), "Foo", "Foo", Country.uk))

      ContactAddress("a" * 35, Some(""), None, "Foo", "Foo", Country.uk)
        .overflowExcessCharacters()
        .shouldBe(ContactAddress("a" * 35, Some(""), None, "Foo", "Foo", Country.uk))

      ContactAddress("a" * 36, None, None, "Foo", "Foo", Country.uk)
        .overflowExcessCharacters()
        .shouldBe(ContactAddress("a" * 35, Some("a"), None, "Foo", "Foo", Country.uk))

      ContactAddress("a" * 36, Some(""), None, "Foo", "Foo", Country.uk)
        .overflowExcessCharacters()
        .shouldBe(ContactAddress("a" * 35, Some("a, "), None, "Foo", "Foo", Country.uk))

      ContactAddress("a" * 100, None, None, "Foo", "Foo", Country.uk)
        .overflowExcessCharacters()
        .shouldBe(ContactAddress("a" * 35, Some("a" * 35), Some("a" * 30), "Foo", "Foo", Country.uk))

      ContactAddress("a" * 150, None, None, "Foo", "Foo", Country.uk)
        .overflowExcessCharacters()
        .shouldBe(ContactAddress("a" * 35, Some("a" * 35), Some("a" * 35), "Foo", "Foo", Country.uk))

      ContactAddress(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin non finibus augue, blandit lacinia lectus. Ut nisi lectus, auctor sed mi quis, suscipit faucibus felis. Aenean in ante interdum, ultricies odio mattis, venenatis ligula. Nulla a sem sit amet orci sollicitudin congue. Donec vitae mauris massa. Aenean in quam laoreet.",
        None,
        None,
        "Foo",
        "Foo",
        Country.uk
      )
        .overflowExcessCharacters()
        .shouldBe(
          ContactAddress(
            "Lorem ipsum dolor sit amet,",
            Some("consectetur adipiscing elit. Proin"),
            Some("non finibus augue, blandit lacinia"),
            "Foo",
            "Foo",
            Country.uk
          )
        )

      ContactAddress(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
        Some(
          "Proin non finibus augue, blandit lacinia lectus. Ut nisi lectus, auctor sed mi quis, suscipit faucibus felis. Aenean in ante interdum, ultricies odio mattis, venenatis ligula. Nulla a sem sit amet orci sollicitudin congue. Donec vitae mauris massa. Aenean in quam laoreet."
        ),
        None,
        "Foo",
        "Foo",
        Country.uk
      )
        .overflowExcessCharacters()
        .shouldBe(
          ContactAddress(
            "Lorem ipsum dolor sit amet,",
            Some("consectetur adipiscing elit. Proin"),
            Some("non finibus augue, blandit lacinia"),
            "Foo",
            "Foo",
            Country.uk
          )
        )

      ContactAddress(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
        Some(
          "Proin non finibus augue, blandit lacinia lectus."
        ),
        Some(
          "Ut nisi lectus, auctor sed mi quis, suscipit faucibus felis. Aenean in ante interdum, ultricies odio mattis, venenatis ligula. Nulla a sem sit amet orci sollicitudin congue. Donec vitae mauris massa. Aenean in quam laoreet."
        ),
        "Foo",
        "Foo",
        Country.uk
      )
        .overflowExcessCharacters()
        .shouldBe(
          ContactAddress(
            "Lorem ipsum dolor sit amet,",
            Some("consectetur adipiscing elit. Proin"),
            Some("non finibus augue, blandit lacinia"),
            "Foo",
            "Foo",
            Country.uk
          )
        )

      ContactAddress(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
        Some(
          "Proin non finibus augue,"
        ),
        Some(
          "blandit lacinia lectus. Ut nisi lectus, auctor sed mi quis, suscipit faucibus felis."
        ),
        "Foo",
        "Foo",
        Country.uk
      )
        .overflowExcessCharacters()
        .shouldBe(
          ContactAddress(
            "Lorem ipsum dolor sit amet,",
            Some("consectetur adipiscing elit. Proin"),
            Some("non finibus augue, blandit lacinia"),
            "Foo",
            "Foo",
            Country.uk
          )
        )

      ContactAddress("Wellington Plaza", Some("Wellington Street"), None, "Leeds", "LS1 4DL", Country.uk)
        .overflowExcessCharacters()
        .shouldBe(ContactAddress("Wellington Plaza", Some("Wellington Street"), None, "Leeds", "LS1 4DL", Country.uk))

      ContactAddress(
        "HM Revenue & Customs Wellington Plaza",
        Some("7 Wellington Street"),
        None,
        "Leeds",
        "LS1 4DL",
        Country.uk
      )
        .overflowExcessCharacters()
        .shouldBe(
          ContactAddress(
            "HM Revenue & Customs Wellington",
            Some("Plaza, 7 Wellington Street"),
            None,
            "Leeds",
            "LS1 4DL",
            Country.uk
          )
        )

      ContactAddress(
        "HM Revenue & Customs Wellington Plaza.",
        Some("7 Wellington Street"),
        None,
        "Leeds",
        "LS1 4DL",
        Country.uk
      )
        .overflowExcessCharacters()
        .shouldBe(
          ContactAddress(
            "HM Revenue & Customs Wellington",
            Some("Plaza. 7 Wellington Street"),
            None,
            "Leeds",
            "LS1 4DL",
            Country.uk
          )
        )

      ContactAddress(
        "HM Revenue & Customs Center at Wellington Plaza",
        Some("7 Wellington Street"),
        None,
        "Leeds",
        "LS1 4DL",
        Country.uk
      )
        .overflowExcessCharacters()
        .shouldBe(
          ContactAddress(
            "HM Revenue & Customs Center at",
            Some("Wellington Plaza, 7 Wellington"),
            Some("Street"),
            "Leeds",
            "LS1 4DL",
            Country.uk
          )
        )
    }

    "remove redundant information" in {
      ContactAddress("", Some(""), None, "Foo", "Foo", Country.uk)
        .removeRedundantInformation()
        .shouldBe(ContactAddress("", None, None, "Foo", "Foo", Country.uk))

      ContactAddress("Foo", Some("Foo"), None, "Foo", "Foo", Country.uk)
        .removeRedundantInformation()
        .shouldBe(ContactAddress("Foo", None, None, "Foo", "Foo", Country.uk))

      ContactAddress("Foo Bar", Some("Bar"), None, "Foo", "Foo", Country.uk)
        .removeRedundantInformation()
        .shouldBe(ContactAddress("Foo Bar", None, None, "Foo", "Foo", Country.uk))

      ContactAddress("Foo", Some("Foo Bar"), None, "Foo", "Foo", Country.uk)
        .removeRedundantInformation()
        .shouldBe(ContactAddress("Foo Bar", None, None, "Foo", "Foo", Country.uk))

      ContactAddress("Foo", Some("FooBar"), None, "Foo", "Foo", Country.uk)
        .removeRedundantInformation()
        .shouldBe(ContactAddress("FooBar", None, None, "Foo", "Foo", Country.uk))

      ContactAddress("Zoo Foo", Some("FooBar"), None, "Foo", "Foo", Country.uk)
        .removeRedundantInformation()
        .shouldBe(ContactAddress("Zoo Foo", Some("FooBar"), None, "Foo", "Foo", Country.uk))

      ContactAddress("FooBar", Some("Bar"), None, "Foo", "Foo", Country.uk)
        .removeRedundantInformation()
        .shouldBe(ContactAddress("FooBar", None, None, "Foo", "Foo", Country.uk))

      ContactAddress("FooBar", Some("Bar Zoo"), None, "Foo", "Foo", Country.uk)
        .removeRedundantInformation()
        .shouldBe(ContactAddress("FooBar", Some("Bar Zoo"), None, "Foo", "Foo", Country.uk))
    }

  }
}
