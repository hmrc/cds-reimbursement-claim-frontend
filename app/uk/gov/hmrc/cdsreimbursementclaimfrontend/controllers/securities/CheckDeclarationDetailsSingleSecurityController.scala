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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.MissingPreferenceCertificate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.ntas
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_declaration_details_single_security

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckDeclarationDetailsSingleSecurityController @Inject() (
  val jcc: JourneyControllerComponents,
  val checkDeclarationDetailsPage: check_declaration_details_single_security
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  private val postAction: Call = routes.CheckDeclarationDetailsSingleSecurityController.submit

  import SecuritiesJourney.Checks.*

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS
        & declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final val show: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      Future.successful(
        journey.getLeadDisplayDeclaration
          .fold(Redirect(routes.EnterMovementReferenceNumberController.show))(declaration =>
            Ok(checkDeclarationDetailsPage(declaration, postAction))
          )
      )
    }

  final val submit: Action[AnyContent] =
    simpleActionReadJourney { journey =>
      if (
        journey.getReasonForSecurity
          .exists(ntas.contains) || journey.getReasonForSecurity.contains(MissingPreferenceCertificate)
      )
        Redirect(routes.HaveDocumentsReadyController.show)
      else
        Redirect(routes.ConfirmFullRepaymentController.showFirst)
    }
}
