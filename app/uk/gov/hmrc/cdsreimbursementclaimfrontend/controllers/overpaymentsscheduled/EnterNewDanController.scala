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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterNewDanMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_new_dan

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterNewDanController @Inject() (
  val jcc: JourneyControllerComponents,
  val newDanPage: enter_new_dan
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsScheduledJourneyBaseController
    with EnterNewDanMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration)

  final override val postAction: Call     = routes.EnterNewDanController.submit
  final override val continueAction: Call = routes.EnterAdditionalDetailsController.show

  final override def modifyJourney(journey: Journey, dan: Dan): Journey =
    journey.submitNewDan(dan)
}
