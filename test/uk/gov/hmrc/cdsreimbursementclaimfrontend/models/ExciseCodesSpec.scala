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

import cats.Functor
import cats.Id
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectAdditionalInformationCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectExciseValue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import scala.util.Random

class ExciseCodesSpec extends AnyWordSpec with Matchers {

  "ExciseCodes" should {
    "Return all excise codes" in {
      val ndrcs = Random
        .shuffle(TaxCodes.excise)
        .take(3)
        .map { code =>
          sample[NdrcDetails].copy(taxType = code.value)
        }
        .toList

      val acc14 = Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
      )

      val journeyAnswers                      =
        OverpaymentsSingleJourneyGenerators.completeJourneyGen.sample
          .getOrElse(OverpaymentsSingleJourneyGenerators.emptyJourney)
          .answers
          .copy(
            movementReferenceNumber = Some(sample[MRN]),
            displayDeclaration = Some(acc14)
          )
      val codes: Set[BasisOfOverpaymentClaim] =
        BasisOfOverpaymentClaim.excludeNorthernIrelandClaims(
          true,
          journeyAnswers.displayDeclaration
        )

      codes.size shouldBe 17
      codes        should contain(IncorrectExciseValue)
      codes        should contain(IncorrectAdditionalInformationCode)
    }

    "Return all excise codes except IncorrectExciseValue" in {
      val ndrcs = List(sample[NdrcDetails].copy(taxType = "A10"))
      val acc14 = Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
      )

      val journeyAnswers =
        OverpaymentsSingleJourneyGenerators.completeJourneyGen.sample
          .getOrElse(OverpaymentsSingleJourneyGenerators.emptyJourney)
          .answers
          .copy(
            movementReferenceNumber = Some(sample[MRN]),
            displayDeclaration = Some(acc14)
          )

      val codes: Set[BasisOfOverpaymentClaim] =
        BasisOfOverpaymentClaim.excludeNorthernIrelandClaims(
          true,
          journeyAnswers.displayDeclaration
        )

      codes.size shouldBe 16
      codes        should contain(IncorrectAdditionalInformationCode)
    }

  }

}
