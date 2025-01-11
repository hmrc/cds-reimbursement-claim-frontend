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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Arbitrary
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimedReimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{AssociatedMRNsClaimsAnswer, ClaimedReimbursementsAnswer}

import java.util.UUID

object ClaimedReimbursementsAnswerGen {
  import TaxCodeGen._

  implicit lazy val arbitraryAmount: Arbitrary[BigDecimal] =
    BigDecimalGen.amountNumberArbitrary

  implicit lazy val arbitraryClaimedReimbursement: Arbitrary[ClaimedReimbursement] =
    Arbitrary(
      for
        taxCode     <- genTaxCode
        paidAmount  <- genBigDecimal
        claimAmount <- genBigDecimal
      yield ClaimedReimbursement(
        taxCode,
        paidAmount,
        claimAmount,
        UUID.randomUUID(),
        ClaimedReimbursement.PaymentMethod.`001`,
        UUID.randomUUID().toString(),
        isFilled = true
      )
    )

  implicit lazy val arbitraryClaimedReimbursementsAnswer: Arbitrary[ClaimedReimbursementsAnswer] =
    GeneratorUtils.gen[ClaimedReimbursementsAnswer]

  implicit lazy val arbitraryAssociatedMRNsClaimsAnswer: Arbitrary[AssociatedMRNsClaimsAnswer] =
    GeneratorUtils.gen[AssociatedMRNsClaimsAnswer]
}
