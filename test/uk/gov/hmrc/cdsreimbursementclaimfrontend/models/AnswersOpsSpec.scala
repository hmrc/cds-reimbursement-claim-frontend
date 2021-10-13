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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import cats.data.NonEmptyList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex

class AnswersOpsSpec extends AnyWordSpec with Matchers {

  "The implicit ops of answers" should {

    "get element at index" in {
      val empty = None
      empty.get(AssociatedMrnIndex(-1)) shouldBe None
      empty.get(AssociatedMrnIndex(0))  shouldBe None
      empty.get(AssociatedMrnIndex(1))  shouldBe None
      empty.get(AssociatedMrnIndex(2))  shouldBe None
      empty.get(AssociatedMrnIndex(3))  shouldBe None

      val single = Some(NonEmptyList("a", Nil))
      single.get(AssociatedMrnIndex(-1)) shouldBe None
      single.get(AssociatedMrnIndex(0))  shouldBe None
      single.get(AssociatedMrnIndex(1))  shouldBe None
      single.get(AssociatedMrnIndex(2))  shouldBe Some("a")
      single.get(AssociatedMrnIndex(3))  shouldBe None

      val multiple = Some(NonEmptyList("a", List("b", "c")))
      multiple.get(AssociatedMrnIndex(-1)) shouldBe None
      multiple.get(AssociatedMrnIndex(0))  shouldBe None
      multiple.get(AssociatedMrnIndex(1))  shouldBe None
      multiple.get(AssociatedMrnIndex(2))  shouldBe Some("a")
      multiple.get(AssociatedMrnIndex(3))  shouldBe Some("b")
      multiple.get(AssociatedMrnIndex(4))  shouldBe Some("c")
      multiple.get(AssociatedMrnIndex(5))  shouldBe None
    }

    "check if element is defined at an index" in {
      val empty = None
      empty.isDefinedAt(AssociatedMrnIndex(-1)) shouldBe false
      empty.isDefinedAt(AssociatedMrnIndex(0))  shouldBe false
      empty.isDefinedAt(AssociatedMrnIndex(1))  shouldBe false
      empty.isDefinedAt(AssociatedMrnIndex(2))  shouldBe false
      empty.isDefinedAt(AssociatedMrnIndex(3))  shouldBe false

      val single = Some(NonEmptyList("a", Nil))
      single.isDefinedAt(AssociatedMrnIndex(-1)) shouldBe false
      single.isDefinedAt(AssociatedMrnIndex(0))  shouldBe false
      single.isDefinedAt(AssociatedMrnIndex(1))  shouldBe false
      single.isDefinedAt(AssociatedMrnIndex(2))  shouldBe true
      single.isDefinedAt(AssociatedMrnIndex(3))  shouldBe false

      val multiple = Some(NonEmptyList("a", List("b", "c")))
      multiple.isDefinedAt(AssociatedMrnIndex(-1)) shouldBe false
      multiple.isDefinedAt(AssociatedMrnIndex(0))  shouldBe false
      multiple.isDefinedAt(AssociatedMrnIndex(1))  shouldBe false
      multiple.isDefinedAt(AssociatedMrnIndex(2))  shouldBe true
      multiple.isDefinedAt(AssociatedMrnIndex(3))  shouldBe true
      multiple.isDefinedAt(AssociatedMrnIndex(4))  shouldBe true
      multiple.isDefinedAt(AssociatedMrnIndex(5))  shouldBe false
    }

    "check if element can be appended at index" in {
      val empty = None
      empty.canAppendAt(AssociatedMrnIndex(-1)) shouldBe false
      empty.canAppendAt(AssociatedMrnIndex(0))  shouldBe false
      empty.canAppendAt(AssociatedMrnIndex(1))  shouldBe false
      empty.canAppendAt(AssociatedMrnIndex(2))  shouldBe true
      empty.canAppendAt(AssociatedMrnIndex(3))  shouldBe false

      val single = Some(NonEmptyList("a", Nil))
      single.canAppendAt(AssociatedMrnIndex(-1)) shouldBe false
      single.canAppendAt(AssociatedMrnIndex(0))  shouldBe false
      single.canAppendAt(AssociatedMrnIndex(1))  shouldBe false
      single.canAppendAt(AssociatedMrnIndex(2))  shouldBe false
      single.canAppendAt(AssociatedMrnIndex(3))  shouldBe true
      single.canAppendAt(AssociatedMrnIndex(4))  shouldBe false
      single.canAppendAt(AssociatedMrnIndex(5))  shouldBe false

      val multiple = Some(NonEmptyList("a", List("b", "c")))
      multiple.canAppendAt(AssociatedMrnIndex(-1)) shouldBe false
      multiple.canAppendAt(AssociatedMrnIndex(0))  shouldBe false
      multiple.canAppendAt(AssociatedMrnIndex(1))  shouldBe false
      multiple.canAppendAt(AssociatedMrnIndex(2))  shouldBe false
      multiple.canAppendAt(AssociatedMrnIndex(3))  shouldBe false
      multiple.canAppendAt(AssociatedMrnIndex(4))  shouldBe false
      multiple.canAppendAt(AssociatedMrnIndex(5))  shouldBe true
      multiple.canAppendAt(AssociatedMrnIndex(6))  shouldBe false
    }

    "return next element index" in {
      val empty = None
      empty.nextIndex shouldBe 0

      val single = Some(NonEmptyList("a", Nil))
      single.nextIndex shouldBe 1

      val multiple = Some(NonEmptyList("a", List("b", "c")))
      multiple.nextIndex shouldBe 3
    }

    "replace or append element at index" in {
      val empty = None
      empty.replaceOrAppend(AssociatedMrnIndex(-1), "z").isLeft shouldBe true
      empty.replaceOrAppend(AssociatedMrnIndex(0), "z").isLeft  shouldBe true
      empty.replaceOrAppend(AssociatedMrnIndex(1), "z").isLeft  shouldBe true
      empty.replaceOrAppend(AssociatedMrnIndex(2), "z")         shouldBe Right(Some(NonEmptyList("z", Nil)))
      empty.replaceOrAppend(AssociatedMrnIndex(3), "z").isLeft  shouldBe true

      val single = Some(NonEmptyList("a", Nil))
      single.replaceOrAppend(AssociatedMrnIndex(-1), "z").isLeft shouldBe true
      single.replaceOrAppend(AssociatedMrnIndex(0), "z").isLeft  shouldBe true
      single.replaceOrAppend(AssociatedMrnIndex(1), "z").isLeft  shouldBe true
      single.replaceOrAppend(AssociatedMrnIndex(2), "z")         shouldBe Right(Some(NonEmptyList("z", Nil)))
      single.replaceOrAppend(AssociatedMrnIndex(3), "z")         shouldBe Right(Some(NonEmptyList("a", List("z"))))
      single.replaceOrAppend(AssociatedMrnIndex(4), "z").isLeft  shouldBe true
      single.replaceOrAppend(AssociatedMrnIndex(5), "z").isLeft  shouldBe true

      val multiple = Some(NonEmptyList("a", List("b", "c")))
      multiple.replaceOrAppend(AssociatedMrnIndex(-1), "z").isLeft shouldBe true
      multiple.replaceOrAppend(AssociatedMrnIndex(0), "z").isLeft  shouldBe true
      multiple.replaceOrAppend(AssociatedMrnIndex(1), "z").isLeft  shouldBe true
      multiple.replaceOrAppend(AssociatedMrnIndex(2), "z")         shouldBe Right(Some(NonEmptyList("z", List("b", "c"))))
      multiple.replaceOrAppend(AssociatedMrnIndex(3), "z")         shouldBe Right(Some(NonEmptyList("a", List("z", "c"))))
      multiple.replaceOrAppend(AssociatedMrnIndex(4), "z")         shouldBe Right(Some(NonEmptyList("a", List("b", "z"))))
      multiple.replaceOrAppend(AssociatedMrnIndex(5), "z")         shouldBe Right(Some(NonEmptyList("a", List("b", "c", "z"))))
      multiple.replaceOrAppend(AssociatedMrnIndex(6), "z").isLeft  shouldBe true
    }

    "list all elements" in {
      val empty = None
      empty.list shouldBe Nil

      val single = Some(NonEmptyList("a", Nil))
      single.list shouldBe List("a")

      val multiple = Some(NonEmptyList("a", List("b", "c")))
      multiple.list shouldBe List("a", "b", "c")
    }

    "list all but the element at index" in {
      val empty = None
      empty.listAllBut(AssociatedMrnIndex(-1)) shouldBe Nil
      empty.listAllBut(AssociatedMrnIndex(0))  shouldBe Nil
      empty.listAllBut(AssociatedMrnIndex(1))  shouldBe Nil
      empty.listAllBut(AssociatedMrnIndex(2))  shouldBe Nil
      empty.listAllBut(AssociatedMrnIndex(3))  shouldBe Nil

      val single = Some(NonEmptyList("a", Nil))
      single.listAllBut(AssociatedMrnIndex(-1)) shouldBe List("a")
      single.listAllBut(AssociatedMrnIndex(0))  shouldBe List("a")
      single.listAllBut(AssociatedMrnIndex(1))  shouldBe List("a")
      single.listAllBut(AssociatedMrnIndex(2))  shouldBe Nil
      single.listAllBut(AssociatedMrnIndex(3))  shouldBe List("a")
      single.listAllBut(AssociatedMrnIndex(4))  shouldBe List("a")
      single.listAllBut(AssociatedMrnIndex(5))  shouldBe List("a")

      val multiple = Some(NonEmptyList("a", List("b", "c")))
      multiple.listAllBut(AssociatedMrnIndex(-1)) shouldBe List("a", "b", "c")
      multiple.listAllBut(AssociatedMrnIndex(0))  shouldBe List("a", "b", "c")
      multiple.listAllBut(AssociatedMrnIndex(1))  shouldBe List("a", "b", "c")
      multiple.listAllBut(AssociatedMrnIndex(2))  shouldBe List("b", "c")
      multiple.listAllBut(AssociatedMrnIndex(3))  shouldBe List("a", "c")
      multiple.listAllBut(AssociatedMrnIndex(4))  shouldBe List("a", "b")
      multiple.listAllBut(AssociatedMrnIndex(5))  shouldBe List("a", "b", "c")
      multiple.listAllBut(AssociatedMrnIndex(6))  shouldBe List("a", "b", "c")
    }

    "remove element at index" in {
      val empty = None
      empty.remove(AssociatedMrnIndex(-1)) shouldBe empty
      empty.remove(AssociatedMrnIndex(0))  shouldBe empty
      empty.remove(AssociatedMrnIndex(1))  shouldBe empty
      empty.remove(AssociatedMrnIndex(2))  shouldBe empty
      empty.remove(AssociatedMrnIndex(3))  shouldBe empty

      val single = Some(NonEmptyList("a", Nil))
      single.remove(AssociatedMrnIndex(-1)) shouldBe single
      single.remove(AssociatedMrnIndex(0))  shouldBe single
      single.remove(AssociatedMrnIndex(1))  shouldBe single
      single.remove(AssociatedMrnIndex(2))  shouldBe None
      single.remove(AssociatedMrnIndex(3))  shouldBe single
      single.remove(AssociatedMrnIndex(4))  shouldBe single
      single.remove(AssociatedMrnIndex(5))  shouldBe single

      val multiple = Some(NonEmptyList("a", List("b", "c")))
      multiple.remove(AssociatedMrnIndex(-1)) shouldBe multiple
      multiple.remove(AssociatedMrnIndex(0))  shouldBe multiple
      multiple.remove(AssociatedMrnIndex(1))  shouldBe multiple
      multiple.remove(AssociatedMrnIndex(2))  shouldBe Some(NonEmptyList("b", List("c")))
      multiple.remove(AssociatedMrnIndex(3))  shouldBe Some(NonEmptyList("a", List("c")))
      multiple.remove(AssociatedMrnIndex(4))  shouldBe Some(NonEmptyList("a", List("b")))
      multiple.remove(AssociatedMrnIndex(5))  shouldBe multiple
      multiple.remove(AssociatedMrnIndex(6))  shouldBe multiple
    }
  }
}
