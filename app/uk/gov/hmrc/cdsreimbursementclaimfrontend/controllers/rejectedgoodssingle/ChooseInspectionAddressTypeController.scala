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
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.inspectionAddressTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Declarant
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Importer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Other
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.problem_with_address
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseInspectionAddressTypeController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val jcc: JourneyControllerComponents,
  val addressLookupService: AddressLookupService,
  inspectionAddressPage: pages.choose_inspection_address_type,
  val problemWithAddressPage: problem_with_address
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext, val errorHandler: ErrorHandler)
    extends RejectedGoodsSingleJourneyBaseController
    with AddressLookup[RejectedGoodsSingleJourney] {

  val applyChoice: Call =
    routes.ChooseInspectionAddressTypeController.submit()

  override val startAddressLookup: Call =
    routes.ChooseInspectionAddressTypeController.redirectToALF()

  override val retrieveLookupAddress: Call =
    routes.ChooseInspectionAddressTypeController.retrieveAddressFromALF()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Monad[Future].map(populateAddresses(journey).asFuture)({
      case Nil =>
        Redirect(startAddressLookup)
      case xs  =>
        Ok(inspectionAddressPage(xs, inspectionAddressTypeForm, applyChoice))
    })
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    inspectionAddressTypeForm
      .bindFromRequest()
      .fold(
        errors =>
          (journey, BadRequest(inspectionAddressPage(populateAddresses(journey), errors, applyChoice))).asFuture,
        {
          case Other     =>
            (journey, Redirect(startAddressLookup)).asFuture
          case Declarant =>
            journey.getDeclarantContactDetailsFromACC14
              .map {
                InspectionAddress.ofType(Declarant).mapFrom(_) |>
                  journey.submitInspectionAddress |>
                  redirectToTheNextPage
              }
              .getOrElse((journey, Redirect(baseRoutes.IneligibleController.ineligible())))
              .asFuture
          case Importer  =>
            journey.getConsigneeContactDetailsFromACC14
              .map {
                InspectionAddress.ofType(Importer).mapFrom(_) |>
                  journey.submitInspectionAddress |>
                  redirectToTheNextPage
              }
              .getOrElse((journey, Redirect(baseRoutes.IneligibleController.ineligible())))
              .asFuture
        }
      )
  }

  override def update(journey: RejectedGoodsSingleJourney): ContactAddress => RejectedGoodsSingleJourney =
    InspectionAddress.ofType(Other).mapFrom(_) |> journey.submitInspectionAddress

  override def redirectToTheNextPage(journey: RejectedGoodsSingleJourney): (RejectedGoodsSingleJourney, Result) =
    if (journey.isAllSelectedDutiesAreCMAEligible)
      (journey, Redirect(routes.ChooseRepaymentMethodController.show()))
    else (journey, Redirect(routes.CheckBankDetailsController.show()))

  private def populateAddresses(journey: RejectedGoodsSingleJourney) = Seq(
    journey.getConsigneeContactDetailsFromACC14.flatMap(_.showAddress).map(Importer  -> _),
    journey.getDeclarantContactDetailsFromACC14.flatMap(_.showAddress).map(Declarant -> _)
  ).flatten(Option.option2Iterable)
}
