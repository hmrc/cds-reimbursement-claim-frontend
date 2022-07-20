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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.WorkInProgressMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_claim

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import play.api.mvc.Action
import play.api.mvc.AnyContent
import com.github.arturopala.validator.Validator.Validate
import SecuritiesJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms

@Singleton
class EnterClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  enterClaimPage: enter_claim
)(implicit viewConfig: ViewConfig, errorHandler: ErrorHandler, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        canContinueTheClaimWithChoosenRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  val key: String = "enter-claim.securities"

  def show(securityDepositId: String, taxCode: TaxCode): Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      val reclaimsForDepositId: Option[SecuritiesJourney.SecuritiesReclaims] =
        journey.answers.securitiesReclaims.flatMap(_.get(securityDepositId))

      (reclaimsForDepositId match {
        case None =>
          if (journey.getSecurityDepositIds.contains(securityDepositId))
            Redirect(routes.SelectDutiesController.show(securityDepositId))
          else
            logAndDisplayError(
              s"Invalid depositId=$securityDepositId. Available deposit IDs",
              journey.getSecurityDepositIds.mkString(",")
            )

        case Some(reclaims) =>
          reclaims.get(taxCode) match {
            case None =>
              Redirect(routes.SelectDutiesController.show(securityDepositId))

            case Some(reclaimAmount) =>
              val totalAmountOnDeclaration =
                journey.getSecurityTaxDetailsFor(securityDepositId, taxCode).map(_.getAmount)
              totalAmountOnDeclaration match {
                case None              =>
                  logAndDisplayError(
                    s"Cannot find the amount of a taxType=${taxCode.value} paid for a depositId=$securityDepositId. Available tax codes",
                    journey.getSecurityTaxCodesFor(securityDepositId).mkString(",")
                  )
                case Some(totalAmount) =>
                  val form = Forms.claimAmountForm(key, totalAmount).withDefault(reclaimAmount)

                  Ok(
                    enterClaimPage(
                      form,
                      securityDepositId,
                      taxCode,
                      totalAmount,
                      routes.EnterClaimController.submit(securityDepositId, taxCode)
                    )
                  )
              }

          }
      }).asFuture
    }

  def submit(securityDepositId: String, taxCode: TaxCode): Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      (journey, NotImplemented).asFuture
    }

}
