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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import com.github.arturopala.validator.Validator._

/** The common base of the claim models
  * @tparam A the type of the claim
  */
abstract class Claim[A : Validate] {
  self: A =>

  /** Case numer is the final result of successfully submitting the claim. */
  def caseNumber: Option[String]

  final val validate: Validate[A] = implicitly[Validate[A]]

  /** Check if the claim is ready to submit, i.e. to get the output. */
  final def hasCompleteAnswers: Boolean =
    validate(this).isValid

  /** Check if the claim has been successfully submitted. */
  final def isFinalized: Boolean = caseNumber.isDefined

  /** Execute the following code only when claim wasn't submitted yet. */
  final def whileClaimIsAmendable(body: => A): A =
    if (isFinalized) this else body

  /** Execute the following code only when claim wasn't submitted yet. */
  final def whileClaimIsAmendable(
    body: => Either[String, A]
  ): Either[String, A] =
    if (isFinalized) Left(JourneyValidationErrors.JOURNEY_ALREADY_FINALIZED)
    else body

}
