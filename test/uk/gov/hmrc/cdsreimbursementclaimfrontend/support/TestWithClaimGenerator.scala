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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.support

import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

trait TestWithClaimGenerator[Claim] {
  self: ScalaCheckPropertyChecks =>

  extension [T <: Tuple](gen: Gen[MRN *: ReasonForSecurity *: DisplayDeclaration *: T]) {
    def withReasonForSecurity(rfs: ReasonForSecurity): Gen[MRN *: ReasonForSecurity *: DisplayDeclaration *: T] =
      gen.map { case mrn *: _ *: declaration *: tup =>
        (mrn, rfs, declaration.withReasonForSecurity(rfs)) ++ tup
      }
  }

  case class ClaimGenerator[TestInput](
    testParamsGenerator: Gen[TestInput],
    claimBuilder: TestInput => Claim
  )

  final def forAllWith[TestParams](
    claimGenerator: ClaimGenerator[TestParams]
  )(test: ((Claim, TestParams)) => Unit)(implicit pos: org.scalactic.source.Position): Unit =
    forAll(claimGenerator.testParamsGenerator) { testParams =>
      test((claimGenerator.claimBuilder(testParams), testParams))
    }

  final def forSomeWith[TestParams](
    claimGenerator: ClaimGenerator[TestParams]
  )(test: ((Claim, TestParams)) => Unit): Unit = {
    val testParams: TestParams = claimGenerator.testParamsGenerator.sample.get
    test((claimGenerator.claimBuilder(testParams), testParams))
  }
}
