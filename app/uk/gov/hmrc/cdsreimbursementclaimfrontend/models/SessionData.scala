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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.CdsVerifiedEmail

final case class SessionData(
  verifiedEmail: Option[CdsVerifiedEmail] = None,
  overpaymentsSingleJourney: Option[OverpaymentsSingleJourney] = None,
  overpaymentsMultipleJourney: Option[OverpaymentsMultipleJourney] = None,
  overpaymentsScheduledJourney: Option[OverpaymentsScheduledJourney] = None,
  rejectedGoodsSingleJourney: Option[RejectedGoodsSingleJourney] = None,
  rejectedGoodsMultipleJourney: Option[RejectedGoodsMultipleJourney] = None,
  rejectedGoodsScheduledJourney: Option[RejectedGoodsScheduledJourney] = None,
  securitiesJourney: Option[SecuritiesJourney] = None
) {

  def withExistingUserData(sessionData: SessionData): SessionData =
    copy(verifiedEmail = sessionData.verifiedEmail)

  override def toString: String = Json.prettyPrint(Json.toJson(this))
}

object SessionData {

  def apply(overpaymentsSingleJourney: OverpaymentsSingleJourney): SessionData =
    SessionData(overpaymentsSingleJourney = Some(overpaymentsSingleJourney))

  def apply(overpaymentsMultipleJourney: OverpaymentsMultipleJourney): SessionData =
    SessionData(overpaymentsMultipleJourney = Some(overpaymentsMultipleJourney))

  def apply(overpaymentsScheduledJourney: OverpaymentsScheduledJourney): SessionData =
    SessionData(overpaymentsScheduledJourney = Some(overpaymentsScheduledJourney))

  def apply(rejectedGoodsSingleJourney: RejectedGoodsSingleJourney): SessionData =
    SessionData(rejectedGoodsSingleJourney = Some(rejectedGoodsSingleJourney))

  def apply(rejectedGoodsMultipleJourney: RejectedGoodsMultipleJourney): SessionData =
    SessionData(rejectedGoodsMultipleJourney = Some(rejectedGoodsMultipleJourney))

  def apply(rejectedGoodsScheduledJourney: RejectedGoodsScheduledJourney): SessionData =
    SessionData(rejectedGoodsScheduledJourney = Some(rejectedGoodsScheduledJourney))

  def apply(securitiesJourney: SecuritiesJourney): SessionData =
    SessionData(securitiesJourney = Some(securitiesJourney))

  implicit val format: Format[SessionData] = Json.format

  implicit val eq: Eq[SessionData] = Eq.fromUniversalEquals[SessionData]

  val empty: SessionData = SessionData()

}
