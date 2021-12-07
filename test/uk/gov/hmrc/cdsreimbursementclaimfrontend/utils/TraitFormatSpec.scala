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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import org.scalacheck.Gen
import org.scalacheck.magnolia._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.TraitFormat

class TraitFormatSpec extends AnyWordSpec with JsonFormatTest with Matchers with ScalaCheckPropertyChecks {

  trait Foo
  case class Foo1(a: String) extends Foo
  case class Foo2(a: Int, b: Boolean) extends Foo
  case class Foo3(s: Seq[Foo2]) extends Foo

  case class Bar(map: Map[String, Foo], option: Option[Foo])

  object Foo extends TraitFormat[Foo] {
    implicit val format1 = Json.format[Foo1]
    implicit val format2 = Json.format[Foo2]
    implicit val format3 = Json.format[Foo3]

    override val members: Seq[Member[_ <: Foo]] = Seq(Member[Foo1], Member[Foo2], Member[Foo3])
  }

  object Bar {
    implicit val format = Json.format[Bar]
  }

  implicit val fooGen: Gen[Foo] = Gen.oneOf[Foo](gen[Foo1].arbitrary, gen[Foo2].arbitrary, gen[Foo3].arbitrary)
  implicit val barGen: Gen[Bar] = for {
    map    <- Gen.mapOf(fooGen.map(v => (Foo.keyOf(v), v)))
    option <- Gen.option(fooGen)
  } yield Bar(map, option)

  "TraitFormat" should {
    "serialize entity into a JSON format" in {
      forAll(barGen) { bar: Bar =>
        validateCanReadAndWriteJson(bar)
      }
    }
  }

}
