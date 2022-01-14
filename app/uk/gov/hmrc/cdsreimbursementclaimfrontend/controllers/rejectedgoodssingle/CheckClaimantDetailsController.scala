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

import cats.data.EitherT
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.DefaultAddressLookupService.isInvalidAddressError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.problem_with_address
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}

import java.util.UUID
import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckClaimantDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  addressLookupService: AddressLookupService,
  claimantDetailsPage: pages.check_claimant_details,
  problemWithAddressPage: problem_with_address
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends RejectedGoodsSingleJourneyBaseController {

  implicit val subKey: Option[String] = None

  val show: Action[AnyContent] = actionReadJourneyAndUser { implicit request => journey => retrievedUserType =>
    val changeCd      = routes.EnterContactDetailsController.show()
    val changeAddress = routes.CheckClaimantDetailsController.startAddressLookup()
    val postAction    = routes.BasisForClaimController.show()
    Future.successful(
      (journey.computeContactDetails(retrievedUserType), journey.computeAddressDetails) match {
        case (Some(cd), Some(ca)) => Ok(claimantDetailsPage(cd, ca, changeCd, changeAddress, postAction))
        case _                    => Redirect(routes.EnterMovementReferenceNumberController.show())
      }
    )
  }

  val startAddressLookup: Action[AnyContent] =
    Action.andThen(jcc.authenticatedAction).async { implicit request =>
      addressLookupService
        .startLookupRedirectingBackTo(routes.CheckClaimantDetailsController.updateAddress())
        .fold(logAndDisplayError("Error occurred starting address lookup: "), url => Redirect(url.toString))
    }

  def updateAddress(addressIdentity: Option[UUID] = None): Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      addressIdentity
        .map(
          addressLookupService
            .retrieveUserAddress(_)
            .map(journey.submitContactAddress)
        )
        .getOrElse(EitherT.rightT[Future, Error](journey))
        .fold(
          error => {
            logger warn s"Error updating Address Lookup address: $error"
            (
              journey,
              if (isInvalidAddressError(error))
                Ok(problemWithAddressPage(routes.CheckClaimantDetailsController.startAddressLookup()))
              else Redirect(baseRoutes.IneligibleController.ineligible())
            )
          },
          journey => (journey, Redirect(routes.CheckClaimantDetailsController.show()))
        )
    }
}

object CheckClaimantDetailsController {
  val checkContactDetailsKey: String = "check-claimant-details"
}
