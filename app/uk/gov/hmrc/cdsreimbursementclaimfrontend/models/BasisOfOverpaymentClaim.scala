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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Id
import cats.implicits.catsSyntaxOption
import cats.kernel.Eq
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.MissingAnswerError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.Validator

sealed abstract class BasisOfOverpaymentClaim extends Product with Serializable

object BasisOfOverpaymentClaim {

  case object DuplicateEntry extends BasisOfOverpaymentClaim
  case object DutySuspension extends BasisOfOverpaymentClaim
  case object EndUseRelief extends BasisOfOverpaymentClaim
  case object IncorrectCommodityCode extends BasisOfOverpaymentClaim
  case object IncorrectCpc extends BasisOfOverpaymentClaim
  case object IncorrectValue extends BasisOfOverpaymentClaim
  case object InwardProcessingReliefFromCustomsDuty extends BasisOfOverpaymentClaim
  case object OutwardProcessingRelief extends BasisOfOverpaymentClaim
  case object PersonalEffects extends BasisOfOverpaymentClaim
  case object Preference extends BasisOfOverpaymentClaim
  case object RGR extends BasisOfOverpaymentClaim
  case object ProofOfReturnRefundGiven extends BasisOfOverpaymentClaim
  case object IncorrectExciseValue extends BasisOfOverpaymentClaim //Northern Ireland only
  case object IncorrectAdditionalInformationCode extends BasisOfOverpaymentClaim //Northern Ireland only
  case object Miscellaneous extends BasisOfOverpaymentClaim

  val validator: Validator[Id, BasisOfOverpaymentClaim] = maybeBasisOfClaim =>
    maybeBasisOfClaim.toValidNel(MissingAnswerError("Basis of claims"))

  implicit val basisOfClaimEquality: Eq[BasisOfOverpaymentClaim] =
    Eq.fromUniversalEquals[BasisOfOverpaymentClaim]

  implicit val basisOfClaimFormat: OFormat[BasisOfOverpaymentClaim] =
    derived.oformat[BasisOfOverpaymentClaim]()
}
