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

import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.SubmitClaimResult.SubmitClaimError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.C285ClaimRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResponse

object SubmissionResponseGen {
  import IdGen._

  implicit lazy val arbitrarySubmissionResponse: Typeclass[SubmitClaimResponse] = gen[SubmitClaimResponse]
  implicit lazy val arbitraryC285ClaimRequest: Typeclass[C285ClaimRequest]      = gen[C285ClaimRequest]
  implicit lazy val arbitrarySubmitClaimError: Typeclass[SubmitClaimError]      = gen[SubmitClaimError]
}
