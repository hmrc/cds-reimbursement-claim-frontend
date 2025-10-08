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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.claims

import cats.Eq
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.ImplicitFluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SeqUtils

/** The common base of the claim model companion objects.
  * @tparam Claim
  *   the type of the claim
  */
trait ClaimCompanion[T] extends ImplicitFluentSyntax[T] with SeqUtils {

  type Features
  type Answers
  type Output

  /** Create an empty initial instance of the claim. */
  def empty(userEoriNumber: Eori, nonce: Nonce = Nonce.random, features: Option[Features] = None): T

  /** Validator of the claim answers consistency and completeness. */
  def validator: Validate[T]

  /** Tries to build claim state out of supplied answers. */
  def tryBuildFrom(answers: Answers, features: Option[Features] = None): Either[String, T]

  implicit final lazy val answersEquality: Eq[Answers] =
    Eq.fromUniversalEquals[Answers]

  implicit final lazy val outputEquality: Eq[Output] =
    Eq.fromUniversalEquals[Output]

  implicit final lazy val equality: Eq[T] =
    Eq.fromUniversalEquals[T]

  implicit final val companion: ClaimCompanion[T] =
    this

}
