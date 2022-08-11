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

import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_claim_details

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import SecuritiesJourney.Checks._

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
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final val show: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      whenAllReclaimsProvided(journey) {
        journey.getLeadDisplayDeclaration
          .fold((journey, Redirect(routes.EnterMovementReferenceNumberController.show()))) { displayDeclaration =>
            (
              journey
                .submitCheckClaimDetailsChangeMode(true)
                .resetClaimFullAmountMode(),
              Ok(
                checkClaimDetailsPage(displayDeclaration, journey.getSecuritiesReclaims, postAction)
              )
            )
          }
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { _ => journey =>
      whenAllReclaimsProvided(journey) {
        (
          journey,
          if (journey.requiresDocumentTypeSelection) {
            Redirect(routes.ChooseFileTypeController.show())
          } else {
            Redirect(routes.UploadFilesController.show())
          }
        )
      }
    }

  private def whenAllReclaimsProvided(
    journey: SecuritiesJourney
  )(body: => (SecuritiesJourney, Result)): Future[(SecuritiesJourney, Result)] =
    (
      if (journey.answers.securitiesReclaims.isEmpty)
        (journey, Redirect(routes.CheckDeclarationDetailsController.show()))
      else
        journey.getNextDepositIdAndTaxCodeToClaim match {
          case Some(Left(depositId)) =>
            (journey, Redirect(routes.ConfirmFullRepaymentController.show(depositId)))

          case Some(Right((depositId, taxCode))) =>
            (journey, Redirect(routes.EnterClaimController.show(depositId, taxCode)))

          case None =>
            body
        }
    ).asFuture

}
