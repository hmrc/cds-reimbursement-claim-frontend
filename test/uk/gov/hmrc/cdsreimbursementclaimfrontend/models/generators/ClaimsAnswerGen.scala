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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Claim, TaxCode, TaxCodes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimsAnswer

object ClaimsAnswerGen {

  implicit lazy val arbitraryTaxCodeGen: Arbitrary[TaxCode] =
    Arbitrary(Gen.oneOf(TaxCodes.all))

  implicit lazy val arbitraryClaim: Typeclass[Claim]               = gen[Claim]
  implicit lazy val arbitraryClaimsAnswer: Typeclass[ClaimsAnswer] = gen[ClaimsAnswer]
}
