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
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithRefund
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest
import RejectedGoodsScheduledJourneyGenerators._

import scala.collection.immutable.SortedMap

class RejectedGoodsScheduledJourneyFormatSpec
    extends AnyWordSpec
    with JsonFormatTest
    with Matchers
    with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "RejectedGoodsScheduledJourney.Answers" should {
    "serialize into a JSON format and back" in {
      validateCanReadAndWriteJson(Answers(userEoriNumber = exampleEori))
      validateCanReadAndWriteJson(
        Answers(userEoriNumber = exampleEori, movementReferenceNumber = Some(MRN("19GB03I52858027001")))
      )
      validateCanReadAndWriteJson(
        Answers(
          userEoriNumber = exampleEori,
          reimbursementClaims = Some(
            SortedMap(
              DutyType.UkDuty -> (SortedMap(
                TaxCode.A00 -> Some(AmountPaidWithRefund(BigDecimal("21.00"), BigDecimal("12.99"))),
                TaxCode.A40 -> None
              )),
              DutyType.EuDuty -> (SortedMap(
                TaxCode.A50 -> Some(AmountPaidWithRefund(BigDecimal("12.99"), BigDecimal("1.01"))),
                TaxCode.A70 -> None
              ))
            )
          )
        )
      )
      validateCanReadAndWriteJson(
        Answers(userEoriNumber = exampleEori, reimbursementClaims = Some(SortedMap()))
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

  "RejectedGoodsScheduledJourney" should {
    "serialize journeys into a JSON format and back" in {
      validateCanReadAndWriteJson(
        RejectedGoodsScheduledJourney.empty(exampleEori)
      )
      validateCanReadAndWriteJson(
        RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(
            MRN("19GB03I52858027001"),
            exampleDisplayDeclaration.withDeclarationId("19GB03I52858027001")
          )
          .getOrFail
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
          Json.parse(Json.stringify(Json.toJson(finalizedJourney))).as[RejectedGoodsScheduledJourney]
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
