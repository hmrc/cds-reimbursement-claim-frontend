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

import cats.Monad
import cats.data.EitherT
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.inspectionAddressTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Declarant
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Importer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Other
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.DefaultAddressLookupService.isInvalidAddressError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.problem_with_address
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseInspectionAddressTypeController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val jcc: JourneyControllerComponents,
  addressLookupService: AddressLookupService,
  inspectionAddressPage: pages.choose_inspection_address_type,
  problemWithAddressPage: problem_with_address
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends RejectedGoodsSingleJourneyBaseController {

  private val showAddressLookupPage   = routes.ChooseInspectionAddressTypeController.redirectToALF()
  private val submitInspectionAddress = routes.ChooseInspectionAddressTypeController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Monad[Future].map(populateAddresses(journey).asFuture)({
      case Nil =>
        Redirect(showAddressLookupPage)
      case xs  =>
        Ok(inspectionAddressPage(xs, inspectionAddressTypeForm, submitInspectionAddress))
    })
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    inspectionAddressTypeForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(inspectionAddressPage(populateAddresses(journey), formWithErrors, submitInspectionAddress))
          ).asFuture,
        { case Other =>
          (journey, Redirect(showAddressLookupPage)).asFuture
        }
      )
  }

  val redirectToALF: Action[AnyContent] =
    Action.andThen(jcc.authenticatedAction).async { implicit request =>
      addressLookupService
        .startLookupRedirectingBackTo(routes.ChooseInspectionAddressTypeController.retrieveAddressFromALF())
        .fold(logAndDisplayError("Error occurred starting address lookup: "), url => Redirect(url.toString))
    }

  def retrieveAddressFromALF(maybeID: Option[UUID] = None): Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      maybeID
        .map { id =>
          addressLookupService
            .retrieveUserAddress(id)
            .map(InspectionAddress.ofType(Other).mapFrom(_))
        }
        .getOrElse(EitherT.leftT[Future, InspectionAddress](Error("The address lookup ID is missing")))
        .fold(
          error => {
            logger warn s"Error retrieving lookup address: $error"
            val errorPage =
              if (isInvalidAddressError(error))
                Ok(problemWithAddressPage(showAddressLookupPage))
              else Redirect(baseRoutes.IneligibleController.ineligible())
            (journey, errorPage)
          },
          updateJourney(journey).andThen(redirect)
        )
    }

  private def populateAddresses(journey: RejectedGoodsSingleJourney) = Seq(
    journey.getConsigneeContactDetailsFromACC14.flatMap(_.showAddress).map(Importer  -> _),
    journey.getDeclarantContactDetailsFromACC14.flatMap(_.showAddress).map(Declarant -> _)
  ).flatten(Option.option2Iterable)

  private def updateJourney(journey: RejectedGoodsSingleJourney): InspectionAddress => RejectedGoodsSingleJourney =
    address => journey.submitInspectionAddress(address)

  private val redirect: RejectedGoodsSingleJourney => (RejectedGoodsSingleJourney, Result) =
    journey => ??? //if (journey.isAllSelectedDutiesAreCMAEligible)

}
