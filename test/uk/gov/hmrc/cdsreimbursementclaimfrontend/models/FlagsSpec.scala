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
import play.api.libs.json.Json

class FlagsSpec extends AnyWordSpec with Matchers {

  "Flags" should {

    "deserialize from string" in {
      val f1 = Flags.parse("11010")
      f1.check(0) shouldBe false
      f1.check(1) shouldBe true
      f1.check(2) shouldBe false
      f1.check(3) shouldBe true
      f1.check(4) shouldBe true
      f1.check(5) shouldBe false
    }

    "serialize to string" in {
      Flags.empty.set(0).set(3).set(4).set(7).toString shouldBe "10011001"
    }

    "set, check and unset single positions" in {
      (0 to 63).foreach { i =>
        val f = Flags.empty
        f.check(i) shouldBe false
        val f2 = f.set(i)
        f2.check(i) shouldBe true
        f2.firstSet shouldBe Some(i)
        val f3 = f2.unset(i)
        f3.check(i)  shouldBe false
        f3.firstSet  shouldBe None
        Json
          .parse(Json.stringify(Json.toJson(f2)))
          .as[Flags] shouldBe f2
      }
    }

    "set and check multiple positions" in {
      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      var f = Flags.empty
      (0 to 63).foreach { i =>
        f.check(i)                       shouldBe false
        f = f.set(i)
        (0 to i).foreach { j =>
          f.check(j)       shouldBe true
          f.checkAllSet(j) shouldBe true
        }
        if (i < 63) f.checkAllSet(i + 1) shouldBe false

        f.firstNotSet shouldBe (if (i < 63) Some(i + 1) else None)
        val f2 = f.unset(i)
        f2.check(i)    shouldBe false
        f2.firstNotSet shouldBe Some(i)
        f.firstSet     shouldBe Some(0)
        Json
          .parse(Json.stringify(Json.toJson(f)))
          .as[Flags]   shouldBe f
      }
    }

    "unset and check multiple positions" in {
      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      var f = Flags.full
      (0 to 63).foreach { i =>
        f.check(i)    shouldBe true
        f = f.unset(i)
        (0 to i).foreach { j =>
          f.check(j)       shouldBe false
          f.checkAllSet(j) shouldBe false
        }
        f.firstNotSet shouldBe Some(0)
        val f2 = f.set(i)
        f2.check(i)    shouldBe true
        f2.firstNotSet shouldBe (if (i > 0) Some(0) else None)
        Json
          .parse(Json.stringify(Json.toJson(f)))
          .as[Flags]   shouldBe f
      }
    }

    "find firstSet" in {
      Flags.empty.firstSet          shouldBe None
      Flags.empty.set(0).firstSet   shouldBe Some(0)
      Flags.empty.set(1).firstSet   shouldBe Some(1)
      Flags.empty.set(62).firstSet  shouldBe Some(62)
      Flags.empty.set(63).firstSet  shouldBe Some(63)
      Flags.full.firstSet           shouldBe Some(0)
      Flags.full.unset(0).firstSet  shouldBe Some(1)
      Flags.full.unset(62).firstSet shouldBe Some(0)
      Flags.full.unset(63).firstSet shouldBe Some(0)
    }

    "find firstNotSet" in {
      Flags.empty.firstNotSet          shouldBe Some(0)
      Flags.empty.set(0).firstNotSet   shouldBe Some(1)
      Flags.empty.set(1).firstNotSet   shouldBe Some(0)
      Flags.full.firstNotSet           shouldBe None
      Flags.full.unset(0).firstNotSet  shouldBe Some(0)
      Flags.full.unset(62).firstNotSet shouldBe Some(62)
      Flags.full.unset(63).firstNotSet shouldBe Some(63)
    }
  }
}
