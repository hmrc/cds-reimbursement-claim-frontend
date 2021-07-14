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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils

import cats.implicits.catsSyntaxOptionId
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim.newDraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils.BasisOfClaimsExamples.{EntryNumberBasisOfClaimsExamples, MrnBasisOfClaimsExamples}

class BasisOfClaimsExamplesSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  "The basis of claims examples" should {
    "drop N items" in {
      val draft = newDraftC285Claim

      forAll(Gen.choose(0, 14), genMovementReferenceNumber) { (n, reference) =>
        val newDraft = draft.copy(movementReferenceNumber = reference.some)
        BasisOfClaimsExamples.of(newDraft).skip(n).items should be((0 to 14).drop(n))
      }
    }

    "build MRN basis of claims examples key" in {
      val examples = MrnBasisOfClaimsExamples(List.empty)

      forAll { n: Int =>
        examples.buildLabelKey(parentKey = "key", n) should be(s"key.details.b$n")
        examples.buildTextKey(parentKey = "key", n)  should be(s"key.details.l$n")
      }
    }

    "build Entry Number basis of claims examples key" in {
      val examples = EntryNumberBasisOfClaimsExamples(List.empty)

      forAll { n: Int =>
        examples.buildLabelKey(parentKey = "key", n) should be(s"key.details.ern.b$n")
        examples.buildTextKey(parentKey = "key", n)  should be(s"key.details.ern.l$n")
      }
    }
  }
}
