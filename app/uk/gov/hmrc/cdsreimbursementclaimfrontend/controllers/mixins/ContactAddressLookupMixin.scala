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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import play.api.mvc._
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ContactDetailsSource.SignedInUserDetails
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ContactDetailsSource.Unknown
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress

trait ContactAddressLookupMixin extends JourneyBaseController with AddressLookupMixin {

  val redirectWhenNoAddressDetailsFound: Call
  val confirmEmailRoute: Call
  val nextPageInTheJourney: Call

  def modifyJourney(journey: Journey, contactDetails: MrnContactDetails): Journey

  def viewTemplate: MrnContactDetails => ContactAddress => Request[_] => HtmlFormat.Appendable

  final val show: Action[AnyContent] = actionReadJourneyAndUser {
    implicit request => journey => authenticatedUser => verifiedEmailOpt =>
      val (maybeContactDetails, maybeAddressDetails) =
        (journey.computeContactDetails(authenticatedUser, verifiedEmailOpt), journey.computeAddressDetails)

//      val emailSource = maybeContactDetails.map(_.source)
//      if (
//        emailSource.contains(SignedInUserDetails) ||
//        emailSource.contains(Unknown)
//      ) {
//        Redirect(confirmEmailRoute).asFuture
//      } else {
      (maybeContactDetails, maybeAddressDetails) match {
        case (Some(cd), _) if cd.emailAddress.isEmpty => Redirect(confirmEmailRoute).asFuture
        case (Some(cd), Some(ca))                     => Ok(viewTemplate(cd)(ca)(request)).asFuture
        case _                                        =>
          logger.warn(
            s"Cannot compute ${maybeContactDetails.map(_ => "").getOrElse("contact details")} ${maybeAddressDetails.map(_ => "").getOrElse("address details")}."
          )
          Redirect(redirectWhenNoAddressDetailsFound).asFuture
      }
//      }

  }

  final val submit: Action[AnyContent] = actionReadWriteJourneyAndUser {
    _ => journey => authenticatedUser => verifiedEmailOpt =>
      (journey.computeContactDetails(authenticatedUser, verifiedEmailOpt), journey.computeAddressDetails) match {
        case (Some(cd), Some(ca)) =>
          (
            modifyJourney(modifyJourney(journey, cd), ca),
            Redirect(nextPageInTheJourney)
          ).asFuture
        case _                    =>
          (
            journey,
            Redirect(redirectWhenNoAddressDetailsFound)
          ).asFuture
      }

  }

}
