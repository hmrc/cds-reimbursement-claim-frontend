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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithRefund

object ReimbursementRejectedGoodsGen {

  lazy val genReimbursement: Gen[AmountPaidWithRefund] = for {
    claimAmount <- genBigDecimal
    random      <- Gen.choose(1, 100)
    paidAmount   = random + claimAmount
  } yield AmountPaidWithRefund(paidAmount, claimAmount)

  implicit lazy val arbitraryReimbursementClaim: Typeclass[AmountPaidWithRefund] =
    Arbitrary(genReimbursement)
}