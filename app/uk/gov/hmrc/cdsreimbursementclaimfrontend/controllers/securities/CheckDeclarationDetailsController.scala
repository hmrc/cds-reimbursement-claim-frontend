/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.implicits.catsSyntaxEq
import cats.syntax.option._
import com.google.inject.Inject
import com.google.inject.Singleton
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_declaration_details

import scala.concurrent.ExecutionContext
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import com.github.arturopala.validator.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val checkDeclarationDetailsPage: check_declaration_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  private val postAction: Call = routes.CheckDeclarationDetailsController.submit

  import SecuritiesJourney.Checks._

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  val show: Action[AnyContent] =
    simpleActionReadWriteJourney { implicit request => journey =>
      val updatedJourney = journey.submitCheckDeclarationDetailsChangeMode(true)
      (
        updatedJourney,
        journey.getLeadDisplayDeclaration
          .fold(Redirect(routes.EnterMovementReferenceNumberController.show()))(declaration =>
            Ok(checkDeclarationDetailsPage(declaration, journey.getSecuritiesReclaims, postAction))
          )
      )
    }

  val submit: Action[AnyContent] =
    simpleActionReadWriteJourney { _ => journey =>
      val updatedJourney = journey.submitCheckDeclarationDetailsChangeMode(false)
      (
        updatedJourney,
        Redirect(
          if (journey.getSelectedDepositIds.isEmpty)
            routes.CheckDeclarationDetailsController.show()
          else if (userHasSeenCYAPage(journey))
            routes.CheckYourAnswersController.show()
          else if (ReasonForSecurity.requiresTotalImportDischarge.map(_.some).contains(journey.getReasonForSecurity))
            routes.CheckTotalImportDischargedController.show()
          else if (ReasonForSecurity.temporaryAdmission.map(_.some).contains(journey.getReasonForSecurity))
            routes.ChooseExportMethodController.show()
          else
            routes.CheckClaimantDetailsController.show()
        )
      )
    }
}
