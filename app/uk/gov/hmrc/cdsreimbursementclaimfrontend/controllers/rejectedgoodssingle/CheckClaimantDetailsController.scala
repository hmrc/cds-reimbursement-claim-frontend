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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckClaimantDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  claimantDetailsPage: pages.check_claimant_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  implicit val subKey: Option[String] = None

  def show(): Action[AnyContent] = simpleActionReadJourneyAndUser { implicit request => journey => retrievedUserType =>
    val changeCd      = routes.EnterContactDetailsController.show()
    val changeAddress = Call("GET", "lookup_address")
    val postAction    = Call("GET", "choose-basis-for-claim")
    Future.successful(
      (journey.getContactDetails(retrievedUserType), journey.getAddressDetails) match {
        case (Some(cd), Some(ca)) => Ok(claimantDetailsPage(cd, ca, changeCd, changeAddress, postAction))
        case _                    => Redirect(routes.EnterMovementReferenceNumberController.show())
      }
    )
  }
}

object CheckClaimantDetailsController {
  val checkContactDetailsKey: String = "check-claimant-details"
}
