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

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex

class AnswersOpsSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  final val nonEmptySequenceOfIntGen: Gen[Option[NonEmptyList[Int]]] =
    Gen.choose(1, 1000).map(l => NonEmptyList.fromList((0 until l).toList))

  final val optionOfNonEmptyListGen: Gen[Option[NonEmptyList[Any]]] =
    Gen.nonEmptyListOf[Any](Gen.oneOf[Any](Gen.alphaChar, Gen.chooseNum(0, 100))).map(NonEmptyList.fromList)

  val anEmpty   = None
  val aSingle   = Some(NonEmptyList("a", Nil))
  val aMultiple = Some(NonEmptyList("a", List("b", "c")))

  "The implicit ops of answers" should {

    "get element at index" in {
      anEmpty.get(AssociatedMrnIndex(-1)) shouldBe None
      anEmpty.get(AssociatedMrnIndex(0))  shouldBe None
      anEmpty.get(AssociatedMrnIndex(1))  shouldBe None
      anEmpty.get(AssociatedMrnIndex(2))  shouldBe None
      anEmpty.get(AssociatedMrnIndex(3))  shouldBe None

      aSingle.get(AssociatedMrnIndex(-1)) shouldBe None
      aSingle.get(AssociatedMrnIndex(0))  shouldBe None
      aSingle.get(AssociatedMrnIndex(1))  shouldBe None
      aSingle.get(AssociatedMrnIndex(2))  shouldBe Some("a")
      aSingle.get(AssociatedMrnIndex(3))  shouldBe None

      aMultiple.get(AssociatedMrnIndex(-1)) shouldBe None
      aMultiple.get(AssociatedMrnIndex(0))  shouldBe None
      aMultiple.get(AssociatedMrnIndex(1))  shouldBe None
      aMultiple.get(AssociatedMrnIndex(2))  shouldBe Some("a")
      aMultiple.get(AssociatedMrnIndex(3))  shouldBe Some("b")
      aMultiple.get(AssociatedMrnIndex(4))  shouldBe Some("c")
      aMultiple.get(AssociatedMrnIndex(5))  shouldBe None

      forAll(nonEmptySequenceOfIntGen) { list =>
        list.get(-1) shouldBe None
        for (i <- 0 until list.length) list.get(i) shouldBe Some(i)
        list.get(list.length)     shouldBe None
        list.get(list.length + 1) shouldBe None
      }
    }

    "check if element is defined at an index" in {
      anEmpty.isDefinedAt(AssociatedMrnIndex(-1)) shouldBe false
      anEmpty.isDefinedAt(AssociatedMrnIndex(0))  shouldBe false
      anEmpty.isDefinedAt(AssociatedMrnIndex(1))  shouldBe false
      anEmpty.isDefinedAt(AssociatedMrnIndex(2))  shouldBe false
      anEmpty.isDefinedAt(AssociatedMrnIndex(3))  shouldBe false

      aSingle.isDefinedAt(AssociatedMrnIndex(-1)) shouldBe false
      aSingle.isDefinedAt(AssociatedMrnIndex(0))  shouldBe false
      aSingle.isDefinedAt(AssociatedMrnIndex(1))  shouldBe false
      aSingle.isDefinedAt(AssociatedMrnIndex(2))  shouldBe true
      aSingle.isDefinedAt(AssociatedMrnIndex(3))  shouldBe false

      aMultiple.isDefinedAt(AssociatedMrnIndex(-1)) shouldBe false
      aMultiple.isDefinedAt(AssociatedMrnIndex(0))  shouldBe false
      aMultiple.isDefinedAt(AssociatedMrnIndex(1))  shouldBe false
      aMultiple.isDefinedAt(AssociatedMrnIndex(2))  shouldBe true
      aMultiple.isDefinedAt(AssociatedMrnIndex(3))  shouldBe true
      aMultiple.isDefinedAt(AssociatedMrnIndex(4))  shouldBe true
      aMultiple.isDefinedAt(AssociatedMrnIndex(5))  shouldBe false

      forAll(optionOfNonEmptyListGen) { list =>
        list.isDefinedAt(-1) shouldBe false
        for (i <- 0 until list.length) list.isDefinedAt(i) shouldBe true
        list.isDefinedAt(list.length)     shouldBe false
        list.isDefinedAt(list.length + 1) shouldBe false
      }
    }

    "check if element can be appended at index" in {
      anEmpty.canAppendAt(AssociatedMrnIndex(-1)) shouldBe false
      anEmpty.canAppendAt(AssociatedMrnIndex(0))  shouldBe false
      anEmpty.canAppendAt(AssociatedMrnIndex(1))  shouldBe false
      anEmpty.canAppendAt(AssociatedMrnIndex(2))  shouldBe true
      anEmpty.canAppendAt(AssociatedMrnIndex(3))  shouldBe false

      aSingle.canAppendAt(AssociatedMrnIndex(-1)) shouldBe false
      aSingle.canAppendAt(AssociatedMrnIndex(0))  shouldBe false
      aSingle.canAppendAt(AssociatedMrnIndex(1))  shouldBe false
      aSingle.canAppendAt(AssociatedMrnIndex(2))  shouldBe false
      aSingle.canAppendAt(AssociatedMrnIndex(3))  shouldBe true
      aSingle.canAppendAt(AssociatedMrnIndex(4))  shouldBe false
      aSingle.canAppendAt(AssociatedMrnIndex(5))  shouldBe false

      aMultiple.canAppendAt(AssociatedMrnIndex(-1)) shouldBe false
      aMultiple.canAppendAt(AssociatedMrnIndex(0))  shouldBe false
      aMultiple.canAppendAt(AssociatedMrnIndex(1))  shouldBe false
      aMultiple.canAppendAt(AssociatedMrnIndex(2))  shouldBe false
      aMultiple.canAppendAt(AssociatedMrnIndex(3))  shouldBe false
      aMultiple.canAppendAt(AssociatedMrnIndex(4))  shouldBe false
      aMultiple.canAppendAt(AssociatedMrnIndex(5))  shouldBe true
      aMultiple.canAppendAt(AssociatedMrnIndex(6))  shouldBe false

      forAll(optionOfNonEmptyListGen) { list =>
        list.canAppendAt(-1) shouldBe false
        for (i <- 0 until list.length) list.canAppendAt(i) shouldBe false
        list.canAppendAt(list.length)     shouldBe true
        list.canAppendAt(list.length + 1) shouldBe false
      }
    }

    "replace or append element at index" in {
      anEmpty.replaceOrAppend(AssociatedMrnIndex(-1), "z").isLeft shouldBe true
      anEmpty.replaceOrAppend(AssociatedMrnIndex(0), "z").isLeft  shouldBe true
      anEmpty.replaceOrAppend(AssociatedMrnIndex(1), "z").isLeft  shouldBe true
      anEmpty.replaceOrAppend(AssociatedMrnIndex(2), "z")         shouldBe Right(Some(NonEmptyList("z", Nil)))
      anEmpty.replaceOrAppend(AssociatedMrnIndex(3), "z").isLeft  shouldBe true

      aSingle.replaceOrAppend(AssociatedMrnIndex(-1), "z").isLeft shouldBe true
      aSingle.replaceOrAppend(AssociatedMrnIndex(0), "z").isLeft  shouldBe true
      aSingle.replaceOrAppend(AssociatedMrnIndex(1), "z").isLeft  shouldBe true
      aSingle.replaceOrAppend(AssociatedMrnIndex(2), "z")         shouldBe Right(Some(NonEmptyList("z", Nil)))
      aSingle.replaceOrAppend(AssociatedMrnIndex(3), "z")         shouldBe Right(Some(NonEmptyList("a", List("z"))))
      aSingle.replaceOrAppend(AssociatedMrnIndex(4), "z").isLeft  shouldBe true
      aSingle.replaceOrAppend(AssociatedMrnIndex(5), "z").isLeft  shouldBe true

      aMultiple.replaceOrAppend(AssociatedMrnIndex(-1), "z").isLeft shouldBe true
      aMultiple.replaceOrAppend(AssociatedMrnIndex(0), "z").isLeft  shouldBe true
      aMultiple.replaceOrAppend(AssociatedMrnIndex(1), "z").isLeft  shouldBe true
      aMultiple.replaceOrAppend(AssociatedMrnIndex(2), "z")         shouldBe Right(Some(NonEmptyList("z", List("b", "c"))))
      aMultiple.replaceOrAppend(AssociatedMrnIndex(3), "z")         shouldBe Right(Some(NonEmptyList("a", List("z", "c"))))
      aMultiple.replaceOrAppend(AssociatedMrnIndex(4), "z")         shouldBe Right(Some(NonEmptyList("a", List("b", "z"))))
      aMultiple.replaceOrAppend(AssociatedMrnIndex(5), "z")         shouldBe Right(Some(NonEmptyList("a", List("b", "c", "z"))))
      aMultiple.replaceOrAppend(AssociatedMrnIndex(6), "z").isLeft  shouldBe true

      forAll(nonEmptySequenceOfIntGen) { list =>
        list.replaceOrAppend(-1, Int.MinValue).isLeft shouldBe true
        for (i <- 0 until list.length) list.replaceOrAppend(i, Int.MinValue).isRight shouldBe true
        list.replaceOrAppend(list.length, Int.MinValue).isRight     shouldBe true
        list.replaceOrAppend(list.length + 1, Int.MinValue).isRight shouldBe false
      }
    }

    "list all elements" in {
      anEmpty.list   shouldBe Nil
      aSingle.list   shouldBe List("a")
      aMultiple.list shouldBe List("a", "b", "c")

      forAll(nonEmptySequenceOfIntGen) { list =>
        val l = list.list
        l.sum shouldBe (l.length * (l.headOption.getOrElse(0) + l.lastOption.getOrElse(0))) / 2
      }
    }

    "report length of list" in {
      anEmpty.length   shouldBe 0
      aSingle.length   shouldBe 1
      aMultiple.length shouldBe 3

      forAll(optionOfNonEmptyListGen) { list =>
        list.length shouldBe list.map(_.toList.length).getOrElse(0)
      }
    }

    "list all but the element at index" in {
      anEmpty.listAllElementsExceptAt(AssociatedMrnIndex(-1)) shouldBe Nil
      anEmpty.listAllElementsExceptAt(AssociatedMrnIndex(0))  shouldBe Nil
      anEmpty.listAllElementsExceptAt(AssociatedMrnIndex(1))  shouldBe Nil
      anEmpty.listAllElementsExceptAt(AssociatedMrnIndex(2))  shouldBe Nil
      anEmpty.listAllElementsExceptAt(AssociatedMrnIndex(3))  shouldBe Nil

      aSingle.listAllElementsExceptAt(AssociatedMrnIndex(-1)) shouldBe List("a")
      aSingle.listAllElementsExceptAt(AssociatedMrnIndex(0))  shouldBe List("a")
      aSingle.listAllElementsExceptAt(AssociatedMrnIndex(1))  shouldBe List("a")
      aSingle.listAllElementsExceptAt(AssociatedMrnIndex(2))  shouldBe Nil
      aSingle.listAllElementsExceptAt(AssociatedMrnIndex(3))  shouldBe List("a")
      aSingle.listAllElementsExceptAt(AssociatedMrnIndex(4))  shouldBe List("a")
      aSingle.listAllElementsExceptAt(AssociatedMrnIndex(5))  shouldBe List("a")

      aMultiple.listAllElementsExceptAt(AssociatedMrnIndex(-1)) shouldBe List("a", "b", "c")
      aMultiple.listAllElementsExceptAt(AssociatedMrnIndex(0))  shouldBe List("a", "b", "c")
      aMultiple.listAllElementsExceptAt(AssociatedMrnIndex(1))  shouldBe List("a", "b", "c")
      aMultiple.listAllElementsExceptAt(AssociatedMrnIndex(2))  shouldBe List("b", "c")
      aMultiple.listAllElementsExceptAt(AssociatedMrnIndex(3))  shouldBe List("a", "c")
      aMultiple.listAllElementsExceptAt(AssociatedMrnIndex(4))  shouldBe List("a", "b")
      aMultiple.listAllElementsExceptAt(AssociatedMrnIndex(5))  shouldBe List("a", "b", "c")
      aMultiple.listAllElementsExceptAt(AssociatedMrnIndex(6))  shouldBe List("a", "b", "c")

      forAll(nonEmptySequenceOfIntGen) { list =>
        val sum: Int = (list.length * (list.get(0).getOrElse(0) + list.get(list.length - 1).getOrElse(0))) / 2

        list.listAllElementsExceptAt(0).sum           shouldBe sum
        list.listAllElementsExceptAt(list.length).sum shouldBe sum

        for (i <- 1 until list.length) {
          val l           = list.listAllElementsExceptAt(i)
          val expectedSum = sum - list.get(i).getOrElse(0)

          l.sum shouldBe expectedSum
        }
      }
    }

    "remove element at index" in {
      anEmpty.remove(AssociatedMrnIndex(-1)) shouldBe anEmpty
      anEmpty.remove(AssociatedMrnIndex(0))  shouldBe anEmpty
      anEmpty.remove(AssociatedMrnIndex(1))  shouldBe anEmpty
      anEmpty.remove(AssociatedMrnIndex(2))  shouldBe anEmpty
      anEmpty.remove(AssociatedMrnIndex(3))  shouldBe anEmpty

      aSingle.remove(AssociatedMrnIndex(-1)) shouldBe aSingle
      aSingle.remove(AssociatedMrnIndex(0))  shouldBe aSingle
      aSingle.remove(AssociatedMrnIndex(1))  shouldBe aSingle
      aSingle.remove(AssociatedMrnIndex(2))  shouldBe None
      aSingle.remove(AssociatedMrnIndex(3))  shouldBe aSingle
      aSingle.remove(AssociatedMrnIndex(4))  shouldBe aSingle
      aSingle.remove(AssociatedMrnIndex(5))  shouldBe aSingle

      aMultiple.remove(AssociatedMrnIndex(-1)) shouldBe aMultiple
      aMultiple.remove(AssociatedMrnIndex(0))  shouldBe aMultiple
      aMultiple.remove(AssociatedMrnIndex(1))  shouldBe aMultiple
      aMultiple.remove(AssociatedMrnIndex(2))  shouldBe Some(NonEmptyList("b", List("c")))
      aMultiple.remove(AssociatedMrnIndex(3))  shouldBe Some(NonEmptyList("a", List("c")))
      aMultiple.remove(AssociatedMrnIndex(4))  shouldBe Some(NonEmptyList("a", List("b")))
      aMultiple.remove(AssociatedMrnIndex(5))  shouldBe aMultiple
      aMultiple.remove(AssociatedMrnIndex(6))  shouldBe aMultiple

      forAll(nonEmptySequenceOfIntGen) { list =>
        val sum: Int = (list.length * (list.get(0).getOrElse(0) + list.get(list.length - 1).getOrElse(0))) / 2

        list.remove(0).list.sum           shouldBe sum
        list.remove(list.length).list.sum shouldBe sum

        for (i <- 1 until list.length) {
          val l           = list.remove(i)
          val expectedSum = sum - list.get(i).getOrElse(0)

          l.list.sum shouldBe expectedSum
        }
      }
    }
  }
}
