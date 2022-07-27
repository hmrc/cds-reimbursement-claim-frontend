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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_claim_details

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import com.github.arturopala.validator.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import SecuritiesJourney.Checks._
import play.api.mvc.Result
import scala.concurrent.Future

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetailsPage: check_claim_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  private val postAction: Call = routes.CheckClaimDetailsController.submit()

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        canContinueTheClaimWithChoosenRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final val show: Action[AnyContent] = 
    actionReadJourney { implicit request => journey =>
      whenAllReclaimsProvided(journey) {
          journey.getLeadDisplayDeclaration
            .fold(Redirect(routes.EnterMovementReferenceNumberController.show())) { displayDeclaration =>
              Ok(
                checkClaimDetailsPage(displayDeclaration, journey.getSecuritiesReclaims, postAction)
              )
            }
      }
    }

  final val submit: Action[AnyContent] = 
    actionReadJourney { _ => journey =>
      whenAllReclaimsProvided(journey) { 
        if (journey.answers.reasonForSecurity.exists(ReasonForSecurity.requiresDocumentType.contains)) {
          Redirect(routes.ChooseFileTypeController.show())
        } else {
          Redirect(routes.UploadFilesController.show())
        }
      }
    }

  private def whenAllReclaimsProvided(journey: SecuritiesJourney)(body: => Result): Future[Result] = 
    (
    if(journey.answers.securitiesReclaims.isEmpty) 
      Redirect(routes.CheckDeclarationDetailsController.show()) 
    else 
      journey.getNextDepositIdAndTaxCodeToClaim match {
        case Some(Left(depositId)) =>
            Redirect(routes.SelectDutiesController.show(depositId)) 

        case Some(Right((depositId, taxCode))) =>
          Redirect(routes.EnterClaimController.show(depositId, taxCode))

        case None =>
          body
      }
    ).asFuture

}
