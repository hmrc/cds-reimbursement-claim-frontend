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

import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.{FillingOutClaim, JustSubmittedClaim, SubmitClaimFailed}
import org.scalacheck.ScalacheckShapeless._

object JourneyStatusGen extends JourneyStatusLowerPriorityGen with GenUtils {
  implicit val journeyStatusGen: Gen[JourneyStatus] = gen[JourneyStatus]
}

trait JourneyStatusLowerPriorityGen { this: GenUtils =>

  implicit val fillingOutClaimGen: Gen[FillingOutClaim] = gen[FillingOutClaim]

  implicit val justSubmittedClaimGen: Gen[JustSubmittedClaim] = gen[JustSubmittedClaim]

  implicit val submitClaimFailedGen: Gen[SubmitClaimFailed] = gen[SubmitClaimFailed]

}
