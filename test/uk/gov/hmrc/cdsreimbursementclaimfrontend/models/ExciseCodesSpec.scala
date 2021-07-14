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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Functor
import cats.Id
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaim._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{DisplayDeclaration, NdrcDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils.BasisOfClaims

import scala.util.Random

class ExciseCodesSpec extends AnyWordSpec with Matchers {

  "ExciseCodes" should {
    "Return only excise codes not related to Northern Ireland" in {
      val draftC285Claim =
        sample[DraftC285Claim].copy(claimNorthernIrelandAnswer = Some(ClaimNorthernIrelandAnswer.No))

      val codes: List[BasisOfClaim] = BasisOfClaims().withoutNorthernIrelandClaimsIfApplies(draftC285Claim)

      codes.size shouldBe 14
      codes        should not contain EvidenceThatGoodsHaveNotEnteredTheEU
      codes        should not contain IncorrectExciseValue
      codes        should not contain CorrectionToRiskClassification
    }

    "Return all excise codes for Northern Ireland" in {
      val ndrcs = Random.shuffle(TaxCode.listOfUKExciseCodeStrings.toList).take(3).map { code =>
        sample[NdrcDetails].copy(taxType = code)
      }

      val acc14 = Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
      )

      val draftC285Claim =
        sample[DraftC285Claim]
          .copy(claimNorthernIrelandAnswer = Some(ClaimNorthernIrelandAnswer.Yes), displayDeclaration = Some(acc14))

      val codes: List[BasisOfClaim] = BasisOfClaims().withoutNorthernIrelandClaimsIfApplies(draftC285Claim)

      codes.size shouldBe 17
      codes        should contain(EvidenceThatGoodsHaveNotEnteredTheEU)
      codes        should contain(IncorrectExciseValue)
      codes        should contain(CorrectionToRiskClassification)
    }

    "Return all excise codes except IncorrectExciseValue for Northern Ireland" in {
      val ndrcs = List(sample[NdrcDetails].copy(taxType = "A10"))
      val acc14 = Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
      )

      val draftC285Claim =
        sample[DraftC285Claim]
          .copy(claimNorthernIrelandAnswer = Some(ClaimNorthernIrelandAnswer.Yes), displayDeclaration = Some(acc14))

      val codes: List[BasisOfClaim] = BasisOfClaims().withoutNorthernIrelandClaimsIfApplies(draftC285Claim)

      codes.size shouldBe 16
      codes        should contain(EvidenceThatGoodsHaveNotEnteredTheEU)
      codes        should contain(CorrectionToRiskClassification)
    }

  }

}
