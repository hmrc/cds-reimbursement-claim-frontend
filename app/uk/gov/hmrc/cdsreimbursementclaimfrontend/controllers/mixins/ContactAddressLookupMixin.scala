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

import play.api.mvc.*
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress

trait ContactAddressLookupMixin extends ClaimBaseController with AddressLookupMixin {

  val redirectWhenNoAddressDetailsFound: Call
  val confirmEmailRoute: Call
  val nextPageInTheClaim: Call
  val startAddressLookup: Call

  def viewTemplate: MrnContactDetails => ContactAddress => Request[?] ?=> HtmlFormat.Appendable

  final val show: Action[AnyContent] = simpleActionReadWriteClaim { claim =>
    val (maybeContactDetails, maybeAddressDetails) = (claim.answers.contactDetails, claim.answers.contactAddress)
    (maybeContactDetails, maybeAddressDetails) match {
      case (Some(cd), Some(ca)) =>
        (claim, Ok(viewTemplate(cd)(ca)))
      case (Some(cd), None)     =>
        (claim, Redirect(startAddressLookup))
      case _                    =>
        logger.warn(
          s"Cannot compute ${maybeContactDetails.map(_ => "").getOrElse("contact details")} ${maybeAddressDetails.map(_ => "").getOrElse("address details")}."
        )
        (claim, Redirect(redirectWhenNoAddressDetailsFound))
    }
  }

  final val submit: Action[AnyContent] = simpleActionReadClaim { claim =>
    if claim.userHasSeenCYAPage then {
      Redirect(checkYourAnswers)
    } else {
      Redirect(nextPageInTheClaim)
    }
  }
}
