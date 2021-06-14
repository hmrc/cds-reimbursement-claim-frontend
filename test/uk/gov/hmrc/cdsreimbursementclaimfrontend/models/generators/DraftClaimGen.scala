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

import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim

object DraftClaimGen extends HigherPriorityDraftClaimGen

trait HigherPriorityDraftClaimGen extends LowerPriorityDraftClaimGen {
  implicit val arbitraryDraftClaimGen: Typeclass[DraftClaim] = gen[DraftClaim]
}

trait LowerPriorityDraftClaimGen {
  implicit val arbitraryDraftC285Claim: Typeclass[DraftC285Claim] = gen[DraftC285Claim]
}
