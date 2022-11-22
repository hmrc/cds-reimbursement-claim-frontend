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

import cats.Eq
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney

final case class SessionData(
  journeyStatus: Option[JourneyStatus] = None,
  overpaymentsSingleJourney: Option[OverpaymentsSingleJourney] = None,
  rejectedGoodsSingleJourney: Option[RejectedGoodsSingleJourney] = None,
  rejectedGoodsMultipleJourney: Option[RejectedGoodsMultipleJourney] = None,
  rejectedGoodsScheduledJourney: Option[RejectedGoodsScheduledJourney] = None,
  securitiesJourney: Option[SecuritiesJourney] = None,
  uploadDocumentsSessionModel: Option[UploadDocumentsSessionModel] = None
) {

  def withUpdatedC285Claim(update: DraftClaim => DraftClaim): SessionData =
    SessionData(journeyStatus.map {
      case FillingOutClaim(ggCredId, signedInUserDetails, draftClaim) =>
        FillingOutClaim(ggCredId, signedInUserDetails, update(draftClaim))

      case other => other
    })

}

object SessionData {

  def apply(status: JourneyStatus): SessionData =
    SessionData(journeyStatus = Some(status))

  def apply(overpaymentsSingleJourney: OverpaymentsSingleJourney): SessionData =
    SessionData(overpaymentsSingleJourney = Some(overpaymentsSingleJourney))

  def apply(rejectedGoodsSingleJourney: RejectedGoodsSingleJourney): SessionData =
    SessionData(rejectedGoodsSingleJourney = Some(rejectedGoodsSingleJourney))

  def apply(rejectedGoodsMultipleJourney: RejectedGoodsMultipleJourney): SessionData =
    SessionData(rejectedGoodsMultipleJourney = Some(rejectedGoodsMultipleJourney))

  def apply(rejectedGoodsScheduledJourney: RejectedGoodsScheduledJourney): SessionData =
    SessionData(rejectedGoodsScheduledJourney = Some(rejectedGoodsScheduledJourney))

  def apply(securitiesJourney: SecuritiesJourney): SessionData =
    SessionData(securitiesJourney = Some(securitiesJourney))

  def apply(uploadDocumentsModel: UploadDocumentsSessionModel): SessionData =
    SessionData(uploadDocumentsSessionModel = Some(uploadDocumentsModel))

  implicit val format: Format[SessionData] = Json.format

  implicit val eq: Eq[SessionData] = Eq.fromUniversalEquals[SessionData]

  val empty: SessionData = SessionData()
}
