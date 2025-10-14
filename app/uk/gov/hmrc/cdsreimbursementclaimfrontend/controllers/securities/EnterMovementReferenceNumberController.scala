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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.movementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_movement_reference_number

import scala.concurrent.ExecutionContext

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val jcc: ClaimControllerComponents,
  enterMovementReferenceNumberPage: enter_movement_reference_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesClaimBaseController {

  final val start: Action[AnyContent] =
    Action(Redirect(routes.EnterMovementReferenceNumberController.show))

  final val show: Action[AnyContent] =
    actionReadClaim { claim =>
      Ok(
        enterMovementReferenceNumberPage(
          movementReferenceNumberForm.withDefault(claim.answers.movementReferenceNumber),
          routes.EnterMovementReferenceNumberController.submit
        )
      ).asFuture
    }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim { implicit request => claim =>
      movementReferenceNumberForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              BadRequest(
                enterMovementReferenceNumberPage(
                  formWithErrors,
                  routes.EnterMovementReferenceNumberController.submit
                )
              )
            ).asFuture,
          mrn =>
            (
              claim.submitMovementReferenceNumber(mrn),
              Redirect(
                if claim.getLeadMovementReferenceNumber.contains(mrn) &&
                  claim.answers.modes.checkDeclarationDetailsChangeMode
                then routes.CheckDeclarationDetailsController.show
                else routes.ChooseReasonForSecurityController.show
              )
            ).asFuture
        )
    }
}
