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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress

trait ContactAddressLookupMixin extends JourneyBaseController with AddressLookupMixin {

  val redirectWhenNoAddressDetailsFound: Call
  val confirmEmailRoute: Call
  val nextPageInTheJourney: Call

  def modifyJourney(journey: Journey, contactDetails: MrnContactDetails): Journey

  def modifyJourney(journey: Journey, enterContactDetailsMode: Boolean): Journey

  def viewTemplate: MrnContactDetails => ContactAddress => Request[_] => HtmlFormat.Appendable

  final val show: Action[AnyContent] = simpleActionReadWriteJourney { implicit request => journey =>
    val (maybeContactDetails, maybeAddressDetails) =
      (journey.answers.contactDetails, journey.answers.contactAddress)
    (maybeContactDetails, maybeAddressDetails) match {
      case (Some(cd), Some(ca)) =>
        (modifyJourney(journey, enterContactDetailsMode = false), Ok(viewTemplate(cd)(ca)(request)))
      case _                    =>
        logger.warn(
          s"Cannot compute ${maybeContactDetails.map(_ => "").getOrElse("contact details")} ${maybeAddressDetails.map(_ => "").getOrElse("address details")}."
        )
        ((modifyJourney(journey, enterContactDetailsMode = false), Redirect(redirectWhenNoAddressDetailsFound)))
    }
  }

  final val submit: Action[AnyContent] = simpleActionReadJourney { journey =>
    if journey.userHasSeenCYAPage then {
      Redirect(checkYourAnswers)
    } else {
      Redirect(nextPageInTheJourney)
    }
  }
}
