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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}

import scala.concurrent.ExecutionContext

abstract class SecuritiesJourneyBaseController(implicit ec: ExecutionContext)
    extends JourneyBaseController[SecuritiesJourney] {

  final override val requiredFeature: Option[Feature] =
    Some(Feature.Securities)

  final override val startOfTheJourney: Call =
    uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start()

  final override val checkYourAnswers: Call            = routes.CheckYourAnswersController.show()
  final override val claimSubmissionConfirmation: Call = routes.CheckYourAnswersController.showConfirmation()

  final override def getJourney(sessionData: SessionData): Option[SecuritiesJourney] =
    sessionData.securitiesJourney

  final override def updateJourney(sessionData: SessionData, journey: SecuritiesJourney): SessionData =
    sessionData.copy(securitiesJourney = Some(journey))

  final override def userHasSeenCYAPage(journey: SecuritiesJourney): Boolean =
    journey.answers.checkYourAnswersChangeMode

  final override def hasCompleteAnswers(journey: SecuritiesJourney): Boolean =
    journey.hasCompleteAnswers

  final override def isFinalized(journey: SecuritiesJourney): Boolean =
    journey.isFinalized

}