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

import com.github.arturopala.validator.Validator.Validate
import com.github.arturopala.validator.Validator.ValidationResultOps

/** The common base of the claim models
  * @tparam A the type of the claim
  */
abstract class JourneyBase[Journey : Validate] {
  self: Journey with CommonJourneyProperties =>

  final val validate: Validate[Journey] = implicitly[Validate[Journey]]

  /** Check if the claim is ready to submit, i.e. to get the output. */
  final def hasCompleteAnswers: Boolean =
    validate(this).isValid

  /** Check if the claim has been successfully submitted. */
  final def isFinalized: Boolean = caseNumber.isDefined

  /** Check if the user has already visited the CYA page at least once. */
  final def userHasSeenCYAPage: Boolean = answers.checkYourAnswersChangeMode

  /** Execute the following code only when claim wasn't submitted yet. */
  final protected def whileClaimIsAmendable(body: => Journey): Journey =
    if (isFinalized) this else body

  /** Execute the following code only when claim wasn't submitted yet. */
  final protected def whileClaimIsAmendable(
    body: => Either[String, Journey]
  ): Either[String, Journey] =
    if (isFinalized) Left(JourneyValidationErrors.JOURNEY_ALREADY_FINALIZED)
    else body

  final protected def whileClaimIsAmendable(condition: Validate[Journey])(body: => Journey): Either[String, Journey] =
    if (isFinalized) Right(this)
    else condition(this).map(_ => body).left.map(_.headMessage)

  /** Execute the following code only when claim wasn't submitted yet and requirements are met. */
  final protected def whileClaimIsAmendableAnd(condition: Validate[Journey])(
    body: => Either[String, Journey]
  ): Either[String, Journey] =
    if (isFinalized) Left(JourneyValidationErrors.JOURNEY_ALREADY_FINALIZED)
    else condition(this).left.map(_.headMessage).flatMap(_ => body)

}
