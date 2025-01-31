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
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.data.Forms.text
import play.api.data.validation.Constraints
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.EnterMovementReferenceNumberController.movementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_movement_reference_number

import scala.concurrent.ExecutionContext

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  enterMovementReferenceNumberPage: enter_movement_reference_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  final val start: Action[AnyContent] =
    Action(Redirect(routes.EnterMovementReferenceNumberController.show))

  final val show: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      Ok(
        enterMovementReferenceNumberPage(
          movementReferenceNumberForm.withDefault(journey.answers.movementReferenceNumber),
          routes.EnterMovementReferenceNumberController.submit
        )
      ).asFuture
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      movementReferenceNumberForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                enterMovementReferenceNumberPage(
                  formWithErrors,
                  routes.EnterMovementReferenceNumberController.submit
                )
              )
            ).asFuture,
          mrn =>
            (
              journey.submitMovementReferenceNumber(mrn),
              Redirect(
                if journey.getLeadMovementReferenceNumber.contains(mrn) &&
                  journey.answers.modes.checkDeclarationDetailsChangeMode
                then routes.CheckDeclarationDetailsController.show
                else routes.ChooseReasonForSecurityController.show
              )
            ).asFuture
        )
    }
}

object EnterMovementReferenceNumberController {

  val enterMovementReferenceNumberKey: String = "enter-movement-reference-number"

  val movementReferenceNumberForm: Form[MRN] =
    Form(
      mapping(
        enterMovementReferenceNumberKey ->
          nonEmptyText
            .verifying(
              "securities.invalid.number",
              str => str.isEmpty || MRN(str).isValid
            )
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )

  val movementReferenceNumberForm2: Form[MRN] =
    Form(
      mapping(
        enterMovementReferenceNumberKey ->
          text
            .verifying(Constraints.nonEmpty(errorMessage = "securities.error.required"))
            .verifying(
              "securities.invalid.number",
              str => str.isEmpty || MRN(str).isValid
            )
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )
}
