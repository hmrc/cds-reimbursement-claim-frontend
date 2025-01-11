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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import com.github.arturopala.validator.Validator._
import play.api.libs.json.Format
import play.api.mvc.Call
import play.api.mvc.Request
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnrolmentConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyValidationErrors.EORI_NOT_ALLOWLISTED
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

trait SecuritiesJourneyBaseController extends JourneyBaseController with SecuritiesJourneyRouter {

  final type Journey = SecuritiesJourney

  final val format: Format[SecuritiesJourney] =
    SecuritiesJourney.format

  final override val requiredFeature: Option[Feature] =
    Some(Feature.Securities)

  final override val startOfTheJourney: Call =
    uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start()

  final override val checkYourAnswers: Call            = routes.CheckYourAnswersController.show
  final override val claimSubmissionConfirmation: Call = routes.CheckYourAnswersController.showConfirmation

  final override def getJourney(sessionData: SessionData): Option[SecuritiesJourney] =
    sessionData.securitiesJourney

  final override def updateJourney(sessionData: SessionData, journey: SecuritiesJourney): SessionData =
    sessionData.copy(securitiesJourney = Some(journey))

  final override def journeyAccessPrecondition(implicit request: Request[_]): Option[Validate[Journey]] =
    if jcc.featureSwitchService.isEnabled(Feature.LimitedAccessSecurities) then Some(validateUserEoriIsOnTheAllowList)
    else None

  private val validateUserEoriIsOnTheAllowList: Validate[SecuritiesJourney] = {
    val accessEoriSet = EnrolmentConfig.getLimitedAccessEoriSet(jcc.configuration)
    checkIsTrue(
      journey => accessEoriSet.contains(journey.answers.userEoriNumber),
      EORI_NOT_ALLOWLISTED
    )
  }
}
