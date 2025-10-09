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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.CdsVerifiedEmail

final case class SessionData(
  verifiedEmail: Option[CdsVerifiedEmail] = None,
  overpaymentsSingleClaim: Option[OverpaymentsSingleClaim] = None,
  overpaymentsMultipleClaim: Option[OverpaymentsMultipleClaim] = None,
  overpaymentsScheduledClaim: Option[OverpaymentsScheduledClaim] = None,
  rejectedGoodsSingleClaim: Option[RejectedGoodsSingleClaim] = None,
  rejectedGoodsMultipleClaim: Option[RejectedGoodsMultipleClaim] = None,
  rejectedGoodsScheduledClaim: Option[RejectedGoodsScheduledClaim] = None,
  securitiesClaim: Option[SecuritiesClaim] = None
) {

  def withExistingUserData(sessionData: SessionData): SessionData =
    copy(verifiedEmail = sessionData.verifiedEmail)

  override def toString: String = Json.prettyPrint(Json.toJson(this))
}

object SessionData {

  def apply(overpaymentsSingleClaim: OverpaymentsSingleClaim): SessionData =
    SessionData(overpaymentsSingleClaim = Some(overpaymentsSingleClaim))

  def apply(overpaymentsMultipleClaim: OverpaymentsMultipleClaim): SessionData =
    SessionData(overpaymentsMultipleClaim = Some(overpaymentsMultipleClaim))

  def apply(overpaymentsScheduledClaim: OverpaymentsScheduledClaim): SessionData =
    SessionData(overpaymentsScheduledClaim = Some(overpaymentsScheduledClaim))

  def apply(rejectedGoodsSingleClaim: RejectedGoodsSingleClaim): SessionData =
    SessionData(rejectedGoodsSingleClaim = Some(rejectedGoodsSingleClaim))

  def apply(rejectedGoodsMultipleClaim: RejectedGoodsMultipleClaim): SessionData =
    SessionData(rejectedGoodsMultipleClaim = Some(rejectedGoodsMultipleClaim))

  def apply(rejectedGoodsScheduledClaim: RejectedGoodsScheduledClaim): SessionData =
    SessionData(rejectedGoodsScheduledClaim = Some(rejectedGoodsScheduledClaim))

  def apply(securitiesClaim: SecuritiesClaim): SessionData =
    SessionData(securitiesClaim = Some(securitiesClaim))

  implicit val format: Format[SessionData] = Json.format

  implicit val eq: Eq[SessionData] = Eq.fromUniversalEquals[SessionData]

  val empty: SessionData = SessionData()

}
