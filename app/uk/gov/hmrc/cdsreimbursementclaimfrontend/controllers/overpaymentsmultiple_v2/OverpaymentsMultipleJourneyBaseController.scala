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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple_v2

import play.api.libs.json.Format
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

trait OverpaymentsMultipleJourneyBaseController extends JourneyBaseController with OverpaymentsMultipleJourneyRouter {

  final type Journey = OverpaymentsMultipleJourney

  final val format: Format[OverpaymentsMultipleJourney] =
    OverpaymentsMultipleJourney.format

  final override val requiredFeature: Option[Feature] =
    Some(Feature.Overpayments_v2)

  final override val startOfTheJourney: Call =
    uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start()

  final override val checkYourAnswers: Call            = routes.CheckYourAnswersController.show
  final override val claimSubmissionConfirmation: Call = routes.CheckYourAnswersController.showConfirmation

  final override def getJourney(sessionData: SessionData): Option[OverpaymentsMultipleJourney] =
    sessionData.overpaymentsMultipleJourney

  final override def updateJourney(sessionData: SessionData, journey: OverpaymentsMultipleJourney): SessionData =
    sessionData.copy(overpaymentsMultipleJourney = Some(journey))

}
