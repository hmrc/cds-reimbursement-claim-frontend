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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.AddressLookupMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.check_claimant_details

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimantDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val addressLookupService: AddressLookupService,
  claimantDetailsPage: check_claimant_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends RejectedGoodsScheduledJourneyBaseController
    with AddressLookupMixin[RejectedGoodsScheduledJourney] {

  val startAddressLookup: Call = routes.CheckClaimantDetailsController.redirectToALF()

  override val problemWithAddressPage: Call = routes.ProblemWithAddressController.show()

  override val retrieveLookupAddress: Call = routes.CheckClaimantDetailsController.retrieveAddressFromALF()

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val show: Action[AnyContent] = actionReadJourneyAndUser { implicit request => journey => retrievedUserType =>
    val changeCd: Call                             = routes.EnterContactDetailsController.show()
    val postAction: Call                           = routes.CheckClaimantDetailsController.submit()
    val (maybeContactDetails, maybeAddressDetails) =
      (journey.computeContactDetails(retrievedUserType), journey.computeAddressDetails)

    (maybeContactDetails, maybeAddressDetails) match {
      case (Some(cd), Some(ca)) =>
        Ok(claimantDetailsPage(cd, ca, changeCd, startAddressLookup, postAction)).asFuture
      case _                    =>
        logger.warn(
          s"${maybeContactDetails.map(_ => "Contact details is defined").getOrElse("Cannot compute contact details")} " +
            s"${maybeAddressDetails.map(_ => "Address details is defined").getOrElse("Cannot compute address details")}"
        )
        Redirect(routes.EnterMovementReferenceNumberController.show()).asFuture
    }

  }

  val submit: Action[AnyContent] = actionReadWriteJourneyAndUser { _ => journey => retrievedUserType =>
    (journey.computeContactDetails(retrievedUserType), journey.computeAddressDetails) match {
      case (Some(cd), Some(ca)) =>
        (
          journey.submitContactDetails(Some(cd)).submitContactAddress(ca),
          Redirect(routes.BasisForClaimController.show())
        ).asFuture
      case _                    =>
        (
          journey,
          Redirect(routes.EnterMovementReferenceNumberController.show())
        ).asFuture
    }

  }

  override def update(journey: RejectedGoodsScheduledJourney): ContactAddress => RejectedGoodsScheduledJourney =
    journey.submitContactAddress

  override def redirectToTheNextPage(journey: RejectedGoodsScheduledJourney): (RejectedGoodsScheduledJourney, Result) =
    (journey, Redirect(routes.CheckClaimantDetailsController.show()))
}
