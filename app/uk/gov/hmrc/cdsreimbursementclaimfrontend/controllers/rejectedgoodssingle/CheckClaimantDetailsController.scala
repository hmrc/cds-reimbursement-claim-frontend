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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.ContactAddressLookupMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney.Checks.hasMRNAndDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.check_claimant_details
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import play.twirl.api.HtmlFormat

@Singleton
class CheckClaimantDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val addressLookupService: AddressLookupService,
  claimantDetailsPage: check_claimant_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends RejectedGoodsSingleJourneyBaseController
    with ContactAddressLookupMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val startAddressLookup: Call =
    routes.CheckClaimantDetailsController.redirectToALF()

  val changeCd: Call =
    routes.EnterContactDetailsController.show()

  val postAction: Call =
    routes.CheckClaimantDetailsController.submit()

  override def viewTemplate: MrnContactDetails => ContactAddress => Request[_] => HtmlFormat.Appendable =
    cd => ca => implicit request => claimantDetailsPage(cd, ca, changeCd, startAddressLookup, postAction)

  override val redirectWhenNoAddressDetailsFound: Call =
    routes.EnterMovementReferenceNumberController.show()

  override val nextPageInTheJourney: Call =
    routes.BasisForClaimController.show()

  override val problemWithAddressPage: Call = routes.ProblemWithAddressController.show()

  override val retrieveLookupAddress: Call =
    routes.CheckClaimantDetailsController.retrieveAddressFromALF()

  // val show: Action[AnyContent] = actionReadJourneyAndUser { implicit request => journey => retrievedUserType =>
  //   val maybeContactDetails = journey.computeContactDetails(retrievedUserType)
  //   val maybeAddressDetails = journey.computeAddressDetails
  //   Future.successful(
  //     (maybeContactDetails, maybeAddressDetails) match {
  //       case (Some(contactDetails), Some(contactAddress)) =>
  //         Ok(claimantDetailsPage(contactDetails, contactAddress, changeContactDetails, startAddressLookup, postAction))
  //       case _                                            =>
  //         logger.warn(
  //           s"Cannot compute ${maybeContactDetails.map(_ => "").getOrElse("contact details")} ${maybeAddressDetails.map(_ => "").getOrElse("address details")}."
  //         )
  //         Redirect(routes.EnterMovementReferenceNumberController.show())
  //     }
  //   )
  // }

  // val submit: Action[AnyContent] = actionReadWriteJourneyAndUser { _ => journey => retrievedUserType =>
  //   Future.successful(
  //     (journey.computeContactDetails(retrievedUserType), journey.computeAddressDetails) match {
  //       case (Some(cd), Some(ca)) =>
  //         (
  //           journey.submitContactDetails(Some(cd)).submitContactAddress(ca),
  //           Redirect(routes.BasisForClaimController.show())
  //         )
  //       case _                    =>
  //         (journey, Redirect(routes.EnterMovementReferenceNumberController.show()))
  //     }
  //   )
  // }

  override def modifyJourney(journey: Journey, contactDetails: MrnContactDetails): Journey =
    journey.submitContactDetails(Some(contactDetails))

  override def modifyJourney(journey: Journey, contactAddress: ContactAddress): Journey =
    journey.submitContactAddress(contactAddress)

  override def redirectToTheNextPage(journey: RejectedGoodsSingleJourney): (RejectedGoodsSingleJourney, Result) =
    (journey, Redirect(routes.CheckClaimantDetailsController.show()))
}
