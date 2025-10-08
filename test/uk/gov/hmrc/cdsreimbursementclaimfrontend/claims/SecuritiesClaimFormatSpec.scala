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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest

import scala.collection.immutable.SortedMap

class SecuritiesClaimFormatSpec extends AnyWordSpec with JsonFormatTest with Matchers with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "SecuritiesClaim.Answers" should {

    "serialize into a JSON format and back" in {
      validateCanReadAndWriteJson(Answers(userEoriNumber = exampleEori))
      validateCanReadAndWriteJson(
        Answers(userEoriNumber = exampleEori, movementReferenceNumber = Some(MRN("19GB03I52858027001")))
      )
      validateCanReadAndWriteJson(
        Answers(
          userEoriNumber = exampleEori,
          correctedAmounts = Some(SortedMap("ABC123" -> SortedMap(TaxCode.A00 -> Some(BigDecimal("12.99")))))
        )
      )
      validateCanReadAndWriteJson(
        Answers(
          userEoriNumber = exampleEori,
          correctedAmounts =
            Some(SortedMap("ABC123" -> SortedMap(TaxCode.A00 -> Some(BigDecimal("12.99")), TaxCode.A40 -> None)))
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

  "SecuritiesClaim" should {
    "serialize claims into a JSON format and back" in {
      validateCanReadAndWriteJson(
        SecuritiesClaim.empty(exampleEori)
      )
      validateCanReadAndWriteJson(
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(MRN("19GB03I52858027001"))
          .submitReasonForSecurityAndDeclaration(
            ReasonForSecurity.AccountSales,
            exampleSecuritiesDisplayDeclaration
              .withDeclarationId("19GB03I52858027001")
              .withReasonForSecurity(ReasonForSecurity.AccountSales)
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
          Json.parse(Json.stringify(Json.toJson(finalizedClaim))).as[SecuritiesClaim]
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
