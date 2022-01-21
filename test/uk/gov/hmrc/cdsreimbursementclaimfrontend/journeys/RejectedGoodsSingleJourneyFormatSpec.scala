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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest

import RejectedGoodsSingleJourneyGenerators._
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce

class RejectedGoodsSingleJourneyFormatSpec
    extends AnyWordSpec
    with JsonFormatTest
    with Matchers
    with ScalaCheckPropertyChecks
    with RejectedGoodsSingleJourneyTestData {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "RejectedGoodsSingleJourney.Answers" should {
    "serialize into a JSON format and back" in {
      validateJsonFormat(s"""{"userEoriNumber":"$exampleEoriAsString"}""", Answers(exampleEori))
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","movementReferenceNumber":"19GB03I52858027001"}""",
        Answers(userEoriNumber = exampleEori, movementReferenceNumber = Some(MRN("19GB03I52858027001")))
      )
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","reimbursementClaims":{"A00":"12.99"}}""",
        Answers(userEoriNumber = exampleEori, reimbursementClaims = Some(Map(TaxCode.A00 -> Some(BigDecimal("12.99")))))
      )
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","reimbursementClaims":{"A00":"12.99","A40":null}}""",
        Answers(
          userEoriNumber = exampleEori,
          reimbursementClaims = Some(Map(TaxCode.A00 -> Some(BigDecimal("12.99")), TaxCode.A40 -> None))
        )
      )
      validateJsonFormat(
        s"""{"userEoriNumber":"$exampleEoriAsString","reimbursementClaims":{}}""",
        Answers(userEoriNumber = exampleEori, reimbursementClaims = Some(Map()))
      )

      validateCanReadAndWriteJson(
        Answers(
          userEoriNumber = exampleEori,
          supportingEvidences = Some(Map(UploadDocumentType.BillOfLading -> ((Nonce.random, Seq(uploadDocument)))))
        )
      )
      validateCanReadAndWriteJson(
        Answers(userEoriNumber = exampleEori, supportingEvidences = Some(Map.empty))
      )
    }
  }

  "RejectedGoodsSingleJourney" should {
    "serialize journeys into a JSON format and back" in {
      validateJsonFormat(
        s"""{"answers":{"userEoriNumber":"$exampleEoriAsString"}}""",
        RejectedGoodsSingleJourney.empty(exampleEori)
      )
      validateJsonFormat(
        s"""{"answers":{"userEoriNumber":"$exampleEoriAsString","movementReferenceNumber":"19GB03I52858027001"}}""",
        RejectedGoodsSingleJourney.empty(exampleEori).submitMovementReferenceNumber(MRN("19GB03I52858027001"))
      )
    }

    "serialize complete journey into a JSON format" in {
      forAll(completeJourneyGen) { journey =>
        validateCanReadAndWriteJson(journey)
      }
    }

    "serialize finalized journey into a JSON format" in {
      forAll(completeJourneyGen) { journey =>
        val finalizedJourney    =
          journey.finalizeJourneyWith("abc-123").getOrElse(fail("cannot finalize the test journey"))
        validateCanReadAndWriteJson(finalizedJourney)
        val deserializedJourney =
          Json.parse(Json.stringify(Json.toJson(finalizedJourney))).as[RejectedGoodsSingleJourney]
        deserializedJourney.caseNumber shouldBe Some("abc-123")
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
