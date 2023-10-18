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
import java.util.UUID
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Hash
import play.api.mvc.Headers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori

class CorrelationIdHeaderSpec extends AnyWordSpec with Matchers {

  "CorrelationIdHeader" should {
    "create X-Correlation-ID header" in {
      CorrelationIdHeader.headerName          shouldBe "X-Correlation-ID"
      CorrelationIdHeader.headerNameLowercase shouldBe "x-correlation-id"

      CorrelationIdHeader("foo-bar") match {
        case ("X-Correlation-ID", v) => v === "foo-bar"
        case _                       => fail()
      }

      CorrelationIdHeader.random() match {
        case ("X-Correlation-ID", v) => UUID.fromString(v)
        case _                       => fail()
      }

      val uuid = UUID.randomUUID()

      CorrelationIdHeader.from(uuid) match {
        case ("X-Correlation-ID", v) => UUID.fromString(v) === uuid
        case _                       => fail()
      }

      CorrelationIdHeader.from(Eori("GB0000000123")) match {
        case ("X-Correlation-ID", v) => assert(v.startsWith(Hash("GB0000000123").take(8)))
        case _                       => fail()
      }

      CorrelationIdHeader.from(Eori("GB0000000123"), uuid) match {
        case ("X-Correlation-ID", v) =>
          assert(v.startsWith(Hash("GB0000000123").take(8)))
          assert(v.contains(uuid.toString.drop(8).take(10)))
        case _                       => fail()
      }

      CorrelationIdHeader.from(Eori("GB0000000123"), Some("foo-bar-0123456789")) match {
        case ("X-Correlation-ID", v) =>
          assert(v.startsWith(Hash("GB0000000123").take(8)))
          assert(v.contains("0123456789"))
        case _                       => fail()
      }

    }
  }

  "HeaderOps" should {
    "add header if missing" in {
      val headers    = new Headers(Seq.empty)
      val newHeaders = CorrelationIdHeader.HeaderOps(headers).addIfMissing(("foo", "bar"))
      newHeaders.getAll("foo") shouldBe Seq("bar")
    }

    "skip adding new header if already present" in {
      val headers    = new Headers(Seq(("foo", "bar")))
      val newHeaders = CorrelationIdHeader.HeaderOps(headers).addIfMissing(("foo", "zoo"))
      newHeaders.getAll("foo") shouldBe Seq("bar")
    }
  }
}
