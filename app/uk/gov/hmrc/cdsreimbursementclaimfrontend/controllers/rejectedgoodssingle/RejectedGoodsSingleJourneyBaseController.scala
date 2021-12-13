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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsingle

import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController

import scala.concurrent.ExecutionContext

abstract class RejectedGoodsSingleJourneyBaseController(
  cc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends JourneyBaseController[RejectedGoodsSingleJourney](cc) {

  override def getJourney(sessionData: SessionData): Option[RejectedGoodsSingleJourney] =
    sessionData.rejectedGoodsSingleJourney

  override def updateJourney(journey: RejectedGoodsSingleJourney)(sessionData: SessionData): SessionData =
    sessionData.copy(rejectedGoodsSingleJourney = Some(journey))

}
