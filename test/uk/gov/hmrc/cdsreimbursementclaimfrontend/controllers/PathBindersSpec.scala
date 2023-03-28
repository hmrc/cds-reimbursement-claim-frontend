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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.mvc.PathBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

class PathBindersSpec extends AnyWordSpec with Matchers {

  implicit val intPathBinder: PathBindable[Int] =
    PathBindable.bindableInt

  "The path binders" should {

    "parse associated mrn index" in {
      PathBinders.associatedMrnIndexPathBinder.bind("key", "-1").isLeft shouldBe true
      PathBinders.associatedMrnIndexPathBinder.bind("key", "0").isLeft  shouldBe true
      PathBinders.associatedMrnIndexPathBinder.bind("key", "1").isLeft  shouldBe true
      PathBinders.associatedMrnIndexPathBinder.bind("key", "2")         shouldBe Right(AssociatedMrnIndex(2))
      PathBinders.associatedMrnIndexPathBinder.bind("key", "3")         shouldBe Right(AssociatedMrnIndex(3))
    }

    "serialize associated mrn index" in {
      PathBinders.associatedMrnIndexPathBinder.unbind("key", AssociatedMrnIndex(-1)) shouldBe "-1"
      PathBinders.associatedMrnIndexPathBinder.unbind("key", AssociatedMrnIndex(0))  shouldBe "0"
      PathBinders.associatedMrnIndexPathBinder.unbind("key", AssociatedMrnIndex(1))  shouldBe "1"
      PathBinders.associatedMrnIndexPathBinder.unbind("key", AssociatedMrnIndex(2))  shouldBe "2"
    }

    "parse and serialize MRN" in {
      PathBinders.mrnBinder.bind("key", "foo")         shouldBe Right(MRN("FOO"))
      PathBinders.mrnBinder.bind("key", "FOO")         shouldBe Right(MRN("FOO"))
      PathBinders.mrnBinder.unbind("key", MRN("foo"))  shouldBe "FOO"
      PathBinders.mrnBinder.unbind("key", MRN("f oo")) shouldBe "FOO"
    }
  }

}
