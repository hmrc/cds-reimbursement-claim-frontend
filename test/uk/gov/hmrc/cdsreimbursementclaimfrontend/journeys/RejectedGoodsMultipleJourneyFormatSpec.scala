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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OrderedMap

class RejectedGoodsMultipleJourneyFormatSpec
    extends AnyWordSpec
    with JsonFormatTest
    with Matchers
    with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "RejectedGoodsMultipleJourney.Answers" should {
    "serialize into a JSON format and back" in {
      validateCanReadAndWriteJson(Answers(userEoriNumber = exampleEori))
      validateCanReadAndWriteJson(
        Answers(
          userEoriNumber = exampleEori,
          movementReferenceNumbers = Some(Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002")))
        )
      )
      validateCanReadAndWriteJson(
        Answers(
          userEoriNumber = exampleEori,
          reimbursementClaims = Some(
            OrderedMap(
              MRN("19GB03I52858027001") -> OrderedMap(TaxCode.A00 -> Some(BigDecimal("12.99"))),
              MRN("19GB03I52858027002") -> OrderedMap(TaxCode.A00 -> None, TaxCode.A20 -> Some(BigDecimal("1.01")))
            )
          )
        )
      )
      validateCanReadAndWriteJson(
        Answers(
          userEoriNumber = exampleEori,
          reimbursementClaims = Some(
            OrderedMap(
              MRN("19GB03I52858027001") -> OrderedMap(TaxCode.A00 -> Some(BigDecimal("12.99")), TaxCode.A40 -> None)
            )
          )
        )
      )
      validateCanReadAndWriteJson(
        Answers(userEoriNumber = exampleEori, reimbursementClaims = Some(OrderedMap()))
      )

      validateCanReadAndWriteJson(
        Answers(
          userEoriNumber = exampleEori,
          supportingEvidences = Seq(uploadDocument)
        )
      )
      validateCanReadAndWriteJson(
        Answers(userEoriNumber = exampleEori, supportingEvidences = Seq.empty)
      )
    }
  }

  "RejectedGoodsMultipleJourney" should {
    "serialize journeys into a JSON format and back" in {
      validateCanReadAndWriteJson(
        RejectedGoodsMultipleJourney.empty(exampleEori)
      )
      validateCanReadAndWriteJson(
        RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(
            0,
            MRN("19GB03I52858027001"),
            buildDisplayDeclaration("19GB03I52858027001")
          )
          .getOrElse(fail("Journey creation has failed"))
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
          Json.parse(Json.stringify(Json.toJson(finalizedJourney))).as[RejectedGoodsMultipleJourney]
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
