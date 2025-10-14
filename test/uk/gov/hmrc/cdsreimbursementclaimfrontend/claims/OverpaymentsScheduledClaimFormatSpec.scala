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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.claims

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest

import scala.collection.immutable.SortedMap

class OverpaymentsScheduledClaimFormatSpec
    extends AnyWordSpec
    with JsonFormatTest
    with Matchers
    with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "OverpaymentsScheduledClaim.Answers" should {
    "serialize into a JSON format and back" in {
      validateCanReadAndWriteJson(Answers(userEoriNumber = exampleEori))
      validateCanReadAndWriteJson(
        Answers(userEoriNumber = exampleEori, movementReferenceNumber = Some(MRN("19GB03I52858027001")))
      )
      validateCanReadAndWriteJson(
        Answers(
          userEoriNumber = exampleEori,
          correctedAmounts = Some(
            SortedMap(
              DutyType.UkDuty -> (SortedMap(
                TaxCode.A00 -> Some(AmountPaidWithCorrect(BigDecimal("21.00"), BigDecimal("12.99"))),
                TaxCode.A40 -> None
              )),
              DutyType.EuDuty -> (SortedMap(
                TaxCode.A50 -> Some(AmountPaidWithCorrect(BigDecimal("12.99"), BigDecimal("1.01"))),
                TaxCode.A70 -> None
              ))
            )
          )
        )
      )
      validateCanReadAndWriteJson(
        Answers(userEoriNumber = exampleEori, correctedAmounts = Some(SortedMap()))
      )

      validateCanReadAndWriteJson(
        Answers(
          userEoriNumber = exampleEori,
          supportingEvidences = Seq(exampleUploadedFile)
        )
      )
      validateCanReadAndWriteJson(
        Answers(userEoriNumber = exampleEori, supportingEvidences = Seq.empty)
      )
    }
  }

  "OverpaymentsScheduledClaim" should {
    "serialize claims into a JSON format and back" in {
      validateCanReadAndWriteJson(
        OverpaymentsScheduledClaim.empty(exampleEori)
      )
      validateCanReadAndWriteJson(
        OverpaymentsScheduledClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(
            MRN("19GB03I52858027001"),
            exampleImportDeclaration.withDeclarationId("19GB03I52858027001")
          )
          .getOrFail
      )
    }

    "serialize complete claim into a JSON format" in {
      forAll(completeClaimGen) { claim =>
        validateCanReadAndWriteJson(claim)
      }
    }

    "serialize finalized claim into a JSON format" in {
      forAll(completeClaimGen) { claim =>
        val finalizedClaim    =
          claim.finalizeClaimWith("abc-123").getOrElse(fail("cannot finalize the test claim"))
        validateCanReadAndWriteJson(finalizedClaim)
        val deserializedClaim =
          Json.parse(Json.stringify(Json.toJson(finalizedClaim))).as[OverpaymentsScheduledClaim]
        deserializedClaim.caseNumber shouldBe Some("abc-123")
      }
    }

    "serialize complete claim's output into a JSON format" in {
      forAll(completeClaimGen) { claim =>
        val output = claim.toOutput.getOrElse(fail("Could not get claim output."))
        validateCanReadAndWriteJson(output)
      }
    }
  }

}
