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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Request
import play.api.mvc.Result
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterMovementReferenceNumberMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.enter_movement_reference_number

import scala.concurrent.ExecutionContext
import play.api.mvc.Action
import play.api.mvc.AnyContent

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  val claimService: ClaimService,
  enterMovementReferenceNumberPage: enter_movement_reference_number
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends OverpaymentsScheduledJourneyBaseController
    with EnterMovementReferenceNumberMixin {

  override def form(journey: Journey): Form[MRN] =
    Forms.movementReferenceNumberForm

  override def getMovementReferenceNumber(journey: Journey): Option[MRN] =
    journey.getLeadMovementReferenceNumber

  override def viewTemplate: Form[MRN] => Request[_] => HtmlFormat.Appendable =
    form =>
      implicit request =>
        enterMovementReferenceNumberPage(
          form,
          Some("overpayments.scheduled"),
          routes.EnterMovementReferenceNumberController.submit
        )

  override def modifyJourney(journey: Journey, mrn: MRN, declaration: DisplayDeclaration): Either[String, Journey] =
    journey.submitMovementReferenceNumberAndDeclaration(mrn, declaration)

  override def afterSuccessfullSubmit(journey: OverpaymentsScheduledJourney): Result =
    if (journey.needsDeclarantAndConsigneeEoriSubmission) {
      Redirect(routes.EnterImporterEoriNumberController.show)
    } else {
      Redirect(routes.CheckDeclarationDetailsController.show)
    }

  final val start: Action[AnyContent] =
    Action(Redirect(routes.EnterMovementReferenceNumberController.show))

}