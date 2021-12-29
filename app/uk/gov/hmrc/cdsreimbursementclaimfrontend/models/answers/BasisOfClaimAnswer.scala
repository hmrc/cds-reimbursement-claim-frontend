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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import cats.Id
import cats.implicits.catsSyntaxOption
import cats.kernel.Eq
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.MissingAnswerError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.Validator

sealed abstract class BasisOfClaimAnswer extends Product with Serializable

object BasisOfClaimAnswer {

  case object DuplicateEntry extends BasisOfClaimAnswer
  case object DutySuspension extends BasisOfClaimAnswer
  case object EndUseRelief extends BasisOfClaimAnswer
  case object IncorrectCommodityCode extends BasisOfClaimAnswer
  case object IncorrectCpc extends BasisOfClaimAnswer
  case object IncorrectValue extends BasisOfClaimAnswer
  case object InwardProcessingReliefFromCustomsDuty extends BasisOfClaimAnswer
  case object OutwardProcessingRelief extends BasisOfClaimAnswer
  case object PersonalEffects extends BasisOfClaimAnswer
  case object Preference extends BasisOfClaimAnswer
  case object RGR extends BasisOfClaimAnswer
  case object ProofOfReturnRefundGiven extends BasisOfClaimAnswer
  case object IncorrectExciseValue extends BasisOfClaimAnswer //Northern Ireland only
  case object IncorrectAdditionalInformationCode extends BasisOfClaimAnswer //Northern Ireland only
  case object Miscellaneous extends BasisOfClaimAnswer

  val validator: Validator[Id, BasisOfClaimAnswer] = maybeBasisOfClaim =>
    maybeBasisOfClaim.toValidNel(MissingAnswerError("Basis of claims"))

  implicit val basisOfClaimAnswerEquality: Eq[BasisOfClaimAnswer] =
    Eq.fromUniversalEquals[BasisOfClaimAnswer]

  implicit val basisOfClaimAnswerFormat: OFormat[BasisOfClaimAnswer] =
    derived.oformat[BasisOfClaimAnswer]()
}
