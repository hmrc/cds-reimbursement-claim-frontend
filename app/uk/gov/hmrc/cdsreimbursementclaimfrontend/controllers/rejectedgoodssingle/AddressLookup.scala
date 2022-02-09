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
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.problem_with_address

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait AddressLookup[Journey] { self: JourneyBaseController[Journey] =>

  implicit val ec: ExecutionContext
  implicit val viewConfig: ViewConfig
  implicit val errorHandler: ErrorHandler

  val addressLookupService: AddressLookupService

  val problemWithAddressPage: problem_with_address

  val startAddressLookup: Call
  val retrieveLookupAddress: Call

  def redirectToALF: Action[AnyContent] =
    Action.andThen(jcc.authenticatedAction).async { implicit request =>
      addressLookupService
        .startLookupRedirectingBackTo(retrieveLookupAddress)
        .fold(logAndDisplayError("Error occurred starting address lookup: "), url => Redirect(url.toString))
    }

  def retrieveAddressFromALF(maybeID: Option[UUID] = None): Action[AnyContent] =
    actionReadWriteJourney(
      { implicit request => journey =>
        maybeID
          .map(addressLookupService.retrieveUserAddress)
          .getOrElse(EitherT.leftT[Future, ContactAddress](Error("The address lookup ID is missing")))
          .fold(
            error => {
              logger warn s"Error retrieving lookup address: $error"
              (
                journey,
                if (
                  error.message.contains("/address/postcode: error.path.missing") || error.message
                    .contains("/address/lines: error.minLength")
                )
                  Ok(problemWithAddressPage(startAddressLookup))
                else Redirect(baseRoutes.IneligibleController.ineligible())
              )
            },
            update(journey) andThen redirectToTheNextPage
          )
      },
      fastForwardToCYAEnabled = false
    )

  def update(journey: Journey): ContactAddress => Journey

  def redirectToTheNextPage(journey: Journey): (Journey, Result)
}
