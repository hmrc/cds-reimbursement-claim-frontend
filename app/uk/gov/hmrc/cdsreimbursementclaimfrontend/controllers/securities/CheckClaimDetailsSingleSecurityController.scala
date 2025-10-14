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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.ntas
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_claim_details_single_security

import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimDetailsSingleSecurityController @Inject() (
  val jcc: ClaimControllerComponents,
  checkClaimDetailsPage: check_claim_details_single_security
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends SecuritiesClaimBaseController {

  private val postAction: Call = routes.CheckClaimDetailsSingleSecurityController.submit

  final override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndImportDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final val show: Action[AnyContent] =
    actionReadWriteClaim { claim =>
      checkIfAllReclaimsProvided(claim) {
        claim.getLeadImportDeclaration
          .fold((claim, Redirect(routes.EnterMovementReferenceNumberController.show))) { importDeclaration =>
            claim.getReclaimWithAmounts.headOption
              .fold((claim, errorHandler.errorResult())) { reclaims =>
                claim.getSecurityDepositIds.headOption.fold(
                  throw new Exception("Security deposit ID expected, but none found")
                ) { firstDepositId =>
                  val availableDuties = claim.getSecurityTaxCodesWithAmounts(firstDepositId)
                  (
                    claim
                      .submitCheckClaimDetailsChangeMode(true)
                      .resetClaimFullAmountMode(),
                    Ok(
                      checkClaimDetailsPage(importDeclaration, reclaims._1, reclaims._2, availableDuties, postAction)
                    )
                  )
                }
              }
          }
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim { claim =>
      checkIfAllReclaimsProvided(claim) {
        (claim, decideNextPage(claim))
      }
    }

  private def checkIfAllReclaimsProvided(
    claim: SecuritiesClaim
  )(body: => (SecuritiesClaim, Result)): (SecuritiesClaim, Result) =
    if claim.answers.correctedAmounts.noneIfEmpty.isEmpty then
      (claim, Redirect(routes.CheckDeclarationDetailsSingleSecurityController.show))
    else
      claim.getNextDepositIdAndTaxCodeToClaim match {
        case Some(Left(depositId)) =>
          (claim, Redirect(routes.ConfirmSingleDepositRepaymentController.show))

        case Some(Right((depositId, taxCode))) =>
          (claim, Redirect(routes.EnterClaimController.show(depositId, taxCode)))

        case None =>
          body
      }

  private def decideNextPage(claim: SecuritiesClaim): Result =
    if claim.userHasSeenCYAPage then Redirect(routes.CheckYourAnswersController.show)
    else if claim.getReasonForSecurity.exists(ntas.contains) then Redirect(routes.ChooseExportMethodController.show)
    else Redirect(routes.ChoosePayeeTypeController.show)
}
