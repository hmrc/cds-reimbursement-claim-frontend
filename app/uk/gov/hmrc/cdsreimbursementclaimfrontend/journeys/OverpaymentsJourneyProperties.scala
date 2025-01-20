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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SubsidiesFeatures

/** Common properties of the overpayments single, multiple and scheduled journeys. */
trait OverpaymentsJourneyProperties extends CommonJourneyProperties {

  override def answers: OverpaymentsAnswers
  def features: Option[SubsidiesFeatures]

  def hasCompleteReimbursementClaims: Boolean
  def getTotalReimbursementAmount: BigDecimal

  def getAvailableClaimTypes(quotaEnabled: Boolean = true): Set[BasisOfOverpaymentClaim]

  final def isSubsidyOnlyJourney: Boolean =
    features.exists(_.shouldAllowSubsidyOnlyPayments) &&
      declarationsHasOnlySubsidyPayments

  final override def validateDeclarationCandidate(declaration: DisplayDeclaration): Option[String] =
    if !(features.exists(_.shouldAllowSubsidyOnlyPayments) &&
        declaration.hasOnlySubsidyPayments) &&
      features.exists(_.shouldBlockSubsidies) &&
      declaration.hasSomeSubsidyPayment
    then Some("error.subsidy-payment-found")
    else None
}
