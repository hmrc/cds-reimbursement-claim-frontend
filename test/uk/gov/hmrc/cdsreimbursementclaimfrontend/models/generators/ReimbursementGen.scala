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

import org.scalacheck.magnolia._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect

object ReimbursementGen {

  lazy val genReimbursement: Gen[AmountPaidWithCorrect] = for {
    shouldPaidAmount <- genBigDecimal
    random           <- Gen.choose(1, 100)
    paidAmount        = random + shouldPaidAmount
  } yield AmountPaidWithCorrect(paidAmount, shouldPaidAmount)

  implicit lazy val arbitraryReimbursementClaim: Typeclass[AmountPaidWithCorrect] =
    Arbitrary(genReimbursement)
}
