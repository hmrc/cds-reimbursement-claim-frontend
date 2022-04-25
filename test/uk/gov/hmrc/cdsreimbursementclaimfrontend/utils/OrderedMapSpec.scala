/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OrderedMapSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  "OrderedMap" should {
    "behave like an immutable Map" in {
      forAll(Gen.mapOf[String, Int](Gen.zip(Gen.alphaNumStr, Gen.chooseNum(0, 99)))) { value: Map[String, Int] =>
        val orderedMap = OrderedMap(value)
        orderedMap.keySet         should contain allElementsOf (value.keySet)
        orderedMap.values         should contain allElementsOf (value.values)
        orderedMap.iterator.toSeq should contain allElementsOf (value.iterator.toSeq)
        (orderedMap + ("foo-123"      -> 1)) should not be orderedMap

        whenever(orderedMap.nonEmpty) {
          val firstKey = value.keys.head
          orderedMap                 should not be OrderedMap.empty[String, Int]
          (orderedMap - firstKey)    should not be orderedMap
          orderedMap.get(firstKey) shouldBe value.get(firstKey)
        }

        for ((k, v) <- value.iterator)
          orderedMap.get(k) shouldBe Some(v)
      }
    }

    "have an empty instance" in {
      OrderedMap.empty[String, Int] shouldBe OrderedMap.empty[String, Int]
    }

    "keep insertion order of items" in {
      OrderedMap("A" -> 1, "B" -> 2, "C" -> 3).iterator.toSeq.mkString(",") shouldBe "(A,1),(B,2),(C,3)"
      OrderedMap("B" -> 2, "A" -> 1, "C" -> 3).iterator.toSeq.mkString(",") shouldBe "(B,2),(A,1),(C,3)"
    }

    "be equal only if all mappings are the same no matter of insertion order" in {
      OrderedMap("B" -> 2, "A" -> 1, "C" -> 3) shouldBe OrderedMap("A" -> 1, "B" -> 2, "C" -> 3)
      OrderedMap("B" -> 2, "A" -> 1, "C" -> 3) should not be OrderedMap("A" -> 1, "B" -> 2, "D" -> 3)
      OrderedMap("B" -> 2, "A" -> 1, "C" -> 3) should not be OrderedMap("A" -> 1, "B" -> 2, "C" -> 4)
      OrderedMap("B" -> 2, "A" -> 1, "C" -> 3) should not be OrderedMap.empty[String, Int]
      OrderedMap.empty[String, Int] should not be OrderedMap("B" -> 2, "A" -> 1, "C" -> 3)
    }
  }

}
