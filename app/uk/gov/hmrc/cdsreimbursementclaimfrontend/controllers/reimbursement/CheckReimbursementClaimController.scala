/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement

import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{boolean, mapping, optional}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class CheckReimbursementClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration
) extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  def showReimbursementClaim(): Action[AnyContent] = Action {
    Ok("implementation todo ....")
  }

  def submitReimbursementClaim(): Action[AnyContent] = Action {
    Ok("implementation todo....")
  }
}

object CheckReimbursementClaimController {

  def checkReimbursementClaimForm: Form[YesNo] = Form(
    mapping(
      "check-reimbursement-claim" -> optional(boolean)
        .verifying("invalid", _.isDefined)
        .transform[YesNo](
          value => if (value.exists(_ === true)) Yes else No,
          answer => Some(answer === Yes)
        )
    )(identity)(Some(_))
  )
}
