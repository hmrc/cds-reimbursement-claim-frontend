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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterContactDetailsMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_or_change_contact_details

import scala.concurrent.ExecutionContext

@Singleton
class EnterContactDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val enterOrChangeContactDetailsPage: enter_or_change_contact_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController
    with EnterContactDetailsMixin {

  final override val postAction: Call =
    routes.EnterContactDetailsController.submit

  final override val continueRoute: Call =
    routes.CheckClaimantDetailsController.show // change to redirectToALF when CheckClaimantDetails page is removed

  final override def modifyJourney(journey: Journey, contactDetails: Option[MrnContactDetails]): Journey =
    journey.submitContactDetails(contactDetails)

}
