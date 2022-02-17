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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.AddressLookupMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.problem_with_address
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimantDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val addressLookupService: AddressLookupService,
  claimantDetailsPage: pages.check_claimant_details,
  val problemWithAddressPage: problem_with_address
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends RejectedGoodsMultipleJourneyBaseController
    with AddressLookupMixin[RejectedGoodsMultipleJourney] {

  implicit val subKey: Option[String] = None

  override val startAddressLookup: Call =
    routes.CheckClaimantDetailsController.redirectToALF()

  override val retrieveLookupAddress: Call =
    routes.CheckClaimantDetailsController.retrieveAddressFromALF()

  val show: Action[AnyContent] = actionReadJourneyAndUser { implicit request => journey => retrievedUserType =>
    val changeCd: Call                             =
      routes.EnterContactDetailsController.show()
    val postAction                                 = routes.CheckClaimantDetailsController.submit()
    val (maybeContactDetails, maybeAddressDetails) =
      (journey.computeContactDetails(retrievedUserType), journey.computeAddressDetails)
    (maybeContactDetails, maybeAddressDetails) match {
      case (Some(cd), Some(ca)) => Ok(claimantDetailsPage(cd, ca, changeCd, startAddressLookup, postAction)).asFuture
      case _                    =>
        logger.warn(
          s"Cannot compute ${maybeContactDetails.map(_ => "").getOrElse("contact details")} ${maybeAddressDetails.map(_ => "").getOrElse("address details")}."
        )
        Redirect(
          routes.EnterMovementReferenceNumberController.show()
        ).asFuture
    }

  }

  val submit: Action[AnyContent] = actionReadWriteJourneyAndUser { _ => journey => retrievedUserType =>
    (journey.computeContactDetails(retrievedUserType), journey.computeAddressDetails) match {
      case (Some(cd), Some(ca)) =>
        (
          journey.submitContactDetails(Some(cd)).submitContactAddress(ca),
          Redirect("/choose-basis-for-claim") //FIXME: routes.BasisForClaimController.show()
        ).asFuture
      case _                    =>
        (
          journey,
          Redirect(routes.EnterMovementReferenceNumberController.show())
        ).asFuture
    }

  }

  override def update(journey: RejectedGoodsMultipleJourney): ContactAddress => RejectedGoodsMultipleJourney =
    journey.submitContactAddress

  override def redirectToTheNextPage(journey: RejectedGoodsMultipleJourney): (RejectedGoodsMultipleJourney, Result) =
    (journey, Redirect(routes.CheckClaimantDetailsController.show()))
}

object CheckClaimantDetailsController {
  val checkContactDetailsKey: String = "check-claimant-details"
}
