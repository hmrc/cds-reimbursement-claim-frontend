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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Request
import play.api.mvc.Result
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterMovementReferenceNumberMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MRNScheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.enter_movement_reference_number
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  val claimService: ClaimService,
  val xiEoriConnector: XiEoriConnector,
  val featureSwitchService: FeatureSwitchService,
  enterMovementReferenceNumberPage: enter_movement_reference_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController
    with EnterMovementReferenceNumberMixin {

  override def isXiEoriSupported(implicit hc: HeaderCarrier): Boolean =
    featureSwitchService.isEnabled(Feature.XiEori)

  override val formKey: String = "enter-movement-reference-number.rejected-goods"

  override def form(journey: Journey): Form[MRN] =
    Forms.movementReferenceNumberRejectedGoodsForm

  override def getMovementReferenceNumber(journey: Journey): Option[MRN] =
    journey.getLeadMovementReferenceNumber

  override def viewTemplate: Form[MRN] => Request[_] => HtmlFormat.Appendable =
    form =>
      implicit request =>
        enterMovementReferenceNumberPage(
          form,
          MRNScheduledRoutes.subKey,
          1,
          routes.EnterMovementReferenceNumberController.submit()
        )

  override def modifyJourney(journey: Journey, mrn: MRN, declaration: DisplayDeclaration): Either[String, Journey] =
    journey.submitMovementReferenceNumberAndDeclaration(mrn, declaration)

  override def modifyJourney(journey: Journey, userXiEori: UserXiEori): Journey =
    journey.submitUserXiEori(userXiEori)

  override def afterSuccessfullSubmit(updatedJourney: RejectedGoodsScheduledJourney): Result =
    Redirect(
      if (updatedJourney.needsDeclarantAndConsigneeEoriSubmission) {
        routes.EnterImporterEoriNumberController.show()
      } else {
        routes.CheckDeclarationDetailsController.show()
      }
    )
}
