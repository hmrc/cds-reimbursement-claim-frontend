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
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.ntas
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_claim_details_single_security

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckClaimDetailsSingleSecurityController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetailsPage: check_claim_details_single_security
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends SecuritiesJourneyBaseController {

  private val postAction: Call = routes.CheckClaimDetailsSingleSecurityController.submit

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final val show: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      checkIfAllReclaimsProvided(journey) {
        journey.getLeadDisplayDeclaration
          .fold((journey, Redirect(routes.EnterMovementReferenceNumberController.show))) { displayDeclaration =>
            journey.getReclaimWithAmounts.headOption
              .fold((journey, errorHandler.errorResult())) { reclaims =>
                journey.getSecurityDepositIds.headOption.fold(
                  throw new Exception("Security deposit ID expected, but none found")
                ) { firstDepositId =>
                  val availableDuties = journey.getSecurityTaxCodesWithAmounts(firstDepositId)
                  (
                    journey
                      .submitCheckClaimDetailsChangeMode(true)
                      .resetClaimFullAmountMode(),
                    Ok(
                      checkClaimDetailsPage(displayDeclaration, reclaims._1, reclaims._2, availableDuties, postAction)
                    )
                  )
                }
              }
          }
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { _ => journey =>
      checkIfAllReclaimsProvided(journey) {
        (journey, decideNextPage(journey))
      }
    }

  private def checkIfAllReclaimsProvided(
    journey: SecuritiesJourney
  )(body: => (SecuritiesJourney, Result)): Future[(SecuritiesJourney, Result)] =
    (
      if journey.answers.correctedAmounts.noneIfEmpty.isEmpty then
        (journey, Redirect(routes.CheckDeclarationDetailsController.show))
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

  private def decideNextPage(journey: SecuritiesJourney): Result =
    if journey.userHasSeenCYAPage then Redirect(routes.CheckYourAnswersController.show)
    else if journey.getReasonForSecurity.exists(ntas.contains) then Redirect(routes.ChooseExportMethodController.show)
    else Redirect(routes.ChoosePayeeTypeController.show)
}
