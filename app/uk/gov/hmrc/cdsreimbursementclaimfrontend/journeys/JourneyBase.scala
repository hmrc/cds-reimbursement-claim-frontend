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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import com.github.arturopala.validator.Validator.Validate
import com.github.arturopala.validator.Validator.ValidationResultOps
import play.api.libs.json.Format
import play.api.libs.json.Json

trait Journey {

  /** Concrete type of the journey */
  type Type
}

/** The common base of the claim models
  * @tparam Journey the type of the claim
  */
trait JourneyBase extends Journey {
  this: CommonJourneyProperties =>

  val self: Type
  implicit val validate: Validate[Type]

  /** Check if the claim is ready to submit, i.e. to get the output. */
  final def hasCompleteAnswers: Boolean =
    validate(self).isValid

  /** Check if the claim has been successfully submitted. */
  final def isFinalized: Boolean = caseNumber.isDefined

  /** Check if the user has already visited the CYA page at least once. */
  final def userHasSeenCYAPage: Boolean = answers.checkYourAnswersChangeMode

  /** Execute the following code only when claim wasn't submitted yet. */
  final protected def whileClaimIsAmendable(body: => Type): Type =
    if (isFinalized) self else body

  /** Execute the following code only when claim wasn't submitted yet. */
  final protected def whileClaimIsAmendable(
    body: => Either[String, Type]
  ): Either[String, Type] =
    if (isFinalized) Left(JourneyValidationErrors.JOURNEY_ALREADY_FINALIZED)
    else body

  final protected def whileClaimIsAmendable(condition: Validate[Type])(body: => Type): Either[String, Type] =
    if (isFinalized) Right(self)
    else condition(self).map(_ => body).left.map(_.headMessage)

  /** Execute the following code only when claim wasn't submitted yet and requirements are met. */
  final protected def whileClaimIsAmendableAnd(condition: Validate[Type])(
    body: => Either[String, Type]
  ): Either[String, Type] =
    if (isFinalized) Left(JourneyValidationErrors.JOURNEY_ALREADY_FINALIZED)
    else condition(self).left.map(_.headMessage).flatMap(_ => body)

  final def prettyPrint(implicit format: Format[Type]): String =
    Json.prettyPrint(Json.toJson[Type](self))

}
