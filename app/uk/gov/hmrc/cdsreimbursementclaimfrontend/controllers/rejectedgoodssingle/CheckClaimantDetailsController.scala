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

import javax.inject.{Inject, Singleton}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{ContactAddress, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.{Email, NamePhoneEmail, PhoneNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckClaimantDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  claimantDetailsPage: pages.check_claimant_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  implicit val subKey: Option[String] = None

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val cd            = journey.answers.contactDetails.getOrElse(
      MrnContactDetails(
        "Alex Smith",
        Email("alex.smith@acmecorporation.com"),
        Some(PhoneNumber("+440234567890"))
      )
    )
    val ca            = journey.answers.contactAddress.getOrElse(
      ContactAddress("73 Guild Street", None, None, "London", "SE23 6FH", Country.uk)
    )
    val changeCd      = Call("GET", "change_contact")
    val changeAddress = Call("GET", "change_address")
    val postAction    = Call("GET", "choose-basis-for-claim")
    Future.successful(
      Ok(claimantDetailsPage(cd, ca, changeCd, changeAddress, postAction))
    )
  }
}

object CheckClaimantDetailsController {
  val checkContactDetailsKey = "check-claimant-details"
}
