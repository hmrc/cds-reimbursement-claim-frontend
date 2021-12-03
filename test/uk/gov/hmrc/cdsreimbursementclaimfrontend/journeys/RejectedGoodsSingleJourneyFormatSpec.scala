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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DocumentTypeRejectedGoods
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest

import RejectedGoodsSingleJourneyGenerators._

class RejectedGoodsSingleJourneyFormatSpec
    extends AnyWordSpec
    with JsonFormatTest
    with Matchers
    with ScalaCheckPropertyChecks
    with RejectedGoodsSingleJourneyTestData {

  "RejectedGoodsSingleJourney.Answers" should {
    "serialize into a JSON format and back" in {
      validateJsonFormat(s"""{"userEoriNumber":"$exampleEoriAsString"}""", Answers(exampleEori))
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","movementReferenceNumber":"19GB03I52858027001"}""",
        Answers(userEoriNumber = exampleEori, movementReferenceNumber = Some(MRN("19GB03I52858027001")))
      )
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","reimbursementClaims":{"A00":12.99}}""",
        Answers(userEoriNumber = exampleEori, reimbursementClaims = Some(Map(TaxCode.A00 -> Some(BigDecimal("12.99")))))
      )
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","reimbursementClaims":{"A00":12.99,"A40":null}}""",
        Answers(
          userEoriNumber = exampleEori,
          reimbursementClaims = Some(Map(TaxCode.A00 -> Some(BigDecimal("12.99")), TaxCode.A40 -> None))
        )
      )
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","reimbursementClaims":{}}""",
        Answers(userEoriNumber = exampleEori, reimbursementClaims = Some(Map()))
      )

      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","supportingEvidences":{"entry__0":{"k":$uploadDocumentJson,"v":{"Foo":{}}}}}""",
        Answers(
          userEoriNumber = exampleEori,
          supportingEvidences = Some(Map(uploadDocument -> Some(DocumentTypeRejectedGoods.Foo)))
        )
      )
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","supportingEvidences":{}}""",
        Answers(userEoriNumber = exampleEori, supportingEvidences = Some(Map()))
      )
    }
  }

  "RejectedGoodsSingleJourney" should {
    "serialize journeys into a JSON format and back" in {
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString"}""",
        RejectedGoodsSingleJourney.empty(exampleEori)
      )
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","movementReferenceNumber":"19GB03I52858027001"}""",
        RejectedGoodsSingleJourney.empty(exampleEori).submitMovementReferenceNumber(MRN("19GB03I52858027001"))
      )
    }

    "serialize complete journey into a JSON format" in {
      forAll(completeJourneyGen) { journey =>
        validateCanReadAndWriteJson(journey)
      }
    }

    "serialize complete journey's output into a JSON format" in {
      forAll(completeJourneyGen) { journey =>
        val output = journey.toOutput.getOrElse(fail("Could not get journey output."))
        validateCanReadAndWriteJson(output)
      }
    }
  }

}
