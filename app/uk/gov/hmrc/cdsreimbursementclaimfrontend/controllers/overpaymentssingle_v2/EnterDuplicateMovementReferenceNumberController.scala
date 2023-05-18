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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2

import com.github.arturopala.validator.Validator.Validate
import play.api.data.Form
import play.api.mvc.Request
import play.api.mvc.Result
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterMovementReferenceNumberMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.enter_duplicate_movement_reference_number
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterDuplicateMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  val claimService: ClaimService,
  val xiEoriConnector: XiEoriConnector,
  featureSwitchService: FeatureSwitchService,
  enterDuplicateMovementReferenceNumberPage: enter_duplicate_movement_reference_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends OverpaymentsSingleJourneyBaseController
    with EnterMovementReferenceNumberMixin {

  override val formKey = "enter-duplicate-movement-reference-number"

  override def isXiEoriSupported(implicit hc: HeaderCarrier): Boolean =
    featureSwitchService.isEnabled(Feature.XiEori)

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsSingleJourney]] =
    Some(
      hasMRNAndDisplayDeclaration &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified &
        needsDuplicateMrnAndDeclaration
    )

  override def form(journey: Journey): Form[MRN] =
    journey.answers.movementReferenceNumber
      .fold(Forms.enterDuplicateMrnWithNoCheck)(Forms.enterDuplicateMrnCheckingAgainst)

  override def getMovementReferenceNumber(journey: Journey): Option[MRN] =
    journey.answers.duplicateDeclaration.map(_.movementReferenceNumber)

  override def viewTemplate: Form[MRN] => Request[_] => HtmlFormat.Appendable =
    form =>
      implicit request =>
        enterDuplicateMovementReferenceNumberPage(
          form,
          Some("overpayments.single"),
          routes.EnterDuplicateMovementReferenceNumberController.submit
        )

  override def modifyJourney(journey: Journey, mrn: MRN, declaration: DisplayDeclaration): Either[String, Journey] =
    journey.submitDuplicateMovementReferenceNumberAndDeclaration(mrn, declaration)

  override def modifyJourney(journey: Journey, userXiEori: UserXiEori): Journey =
    journey.submitUserXiEori(userXiEori)

  override def needsUserXiEoriSubmission(journey: Journey): Boolean =
    journey.needsUserXiEoriSubmissionForDuplicateDeclaration

  override def afterSuccessfullSubmit(journey: OverpaymentsSingleJourney): Result =
    if (journey.needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration) {
      Redirect(routes.EnterImporterEoriNumberOfDuplicateDeclaration.show)
    } else {
      Redirect(routes.CheckDuplicateDeclarationDetailsController.show)
    }

}
