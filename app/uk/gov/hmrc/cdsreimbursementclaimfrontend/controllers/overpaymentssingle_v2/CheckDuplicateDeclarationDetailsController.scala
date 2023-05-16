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
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc._
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.CheckDeclarationDetailsMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.check_declaration_details

import scala.concurrent.ExecutionContext

@Singleton
class CheckDuplicateDeclarationDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkDeclarationDetailsPage: check_declaration_details
)(implicit val viewConfig: ViewConfig, val errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends OverpaymentsSingleJourneyBaseController
    with CheckDeclarationDetailsMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsSingleJourney]] =
    Some(
      hasMRNAndDisplayDeclaration &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified &
        needsDuplicateMrnAndDeclaration &
        hasDuplicateDeclarationVerifiedIfRequired
    )

  final override def getDisplayDeclaration(journey: Journey): Option[DisplayDeclaration] =
    journey.answers.duplicateDeclaration.map(_.displayDeclaration)

  final override def continueRoute(journey: Journey): Call =
    routes.EnterAdditionalDetailsController.show

  final override val enterMovementReferenceNumberRoute: Call =
    routes.EnterDuplicateMovementReferenceNumberController.submit

  private val postAction: Call =
    routes.CheckDuplicateDeclarationDetailsController.submit

  final override def viewTemplate: (DisplayDeclaration, Form[YesNo]) => Request[_] => HtmlFormat.Appendable = {
    case (decl, form) =>
      implicit request =>
        checkDeclarationDetailsPage(decl, form, true, postAction, None)
  }

}
