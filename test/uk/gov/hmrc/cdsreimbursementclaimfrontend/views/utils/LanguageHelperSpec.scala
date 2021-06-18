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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils.LanguageHelper._

class LanguageHelperSpec extends AnyWordSpec with Matchers {

  "LanguageHelper" should {
    "work when there are no journey keys / subKeys / level2 keys" in {
      lang("a", None, "b") shouldBe List("a.b")
    }

    "Return 2 sets of keys" in {
      lang("a", Some("b"), "c") shouldBe List("a.b.c", "a.c")
    }
  }
}
