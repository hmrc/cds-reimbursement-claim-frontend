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

import cats.syntax.eq._
import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_claim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  enterClaimPage: enter_claim
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  private val key: String = "enter-claim.securities"

  final def showFirst(securityDepositId: String): Action[AnyContent] = simpleActionReadJourney(journey =>
    journey
      .getSelectedDutiesFor(securityDepositId)
      .flatMap(_.headOption) match {

      case Some(firstSelectedDuty) =>
        Redirect(routes.EnterClaimController.show(securityDepositId, firstSelectedDuty))

      case None =>
        Redirect(routes.ConfirmFullRepaymentController.show(securityDepositId))
    }
  )

  final def show(securityDepositId: String, taxCode: TaxCode): Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      validateDepositIdAndTaxCode(journey, securityDepositId, taxCode).map(
        _.fold(
          identity,
          { case (existingCorrectedAmountOpt, totalAmount) =>
            val form = Forms
              .claimAmountForm(key, totalAmount)
              .withDefault(existingCorrectedAmountOpt)

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
        )
      )
    }

  final def submit(securityDepositId: String, taxCode: TaxCode): Action[AnyContent] =
    actionReadWriteJourney(
      { implicit request => journey =>
        validateDepositIdAndTaxCode(journey, securityDepositId, taxCode).map(
          _.fold(
            result => (journey, result),
            { case (_, totalAmount) =>
              val form = Forms.claimAmountForm(key, totalAmount)
              form
                .bindFromRequest()
                .fold(
                  formWithErrors =>
                    (
                      journey,
                      BadRequest(
                        enterClaimPage(
                          formWithErrors,
                          securityDepositId,
                          taxCode,
                          totalAmount,
                          routes.EnterClaimController.submit(securityDepositId, taxCode)
                        )
                      )
                    ),
                  reclaimAmount => {
                    val amountHasChanged: Boolean =
                      !journey
                        .getReclaimAmountFor(securityDepositId, taxCode)
                        .exists(_ === reclaimAmount)
                    if (amountHasChanged)
                      journey
                        .submitCorrectAmount(securityDepositId, taxCode, reclaimAmount)
                        .fold(
                          error =>
                            (
                              journey,
                              Redirect(routeForValidationError(error))
                            ),
                          updatedJourney =>
                            (
                              updatedJourney,
                              Redirect(nextPage(updatedJourney, securityDepositId, taxCode, amountHasChanged = true))
                            )
                        )
                    else
                      (journey, Redirect(nextPage(journey, securityDepositId, taxCode, amountHasChanged = false)))

                  }
                )
            }
          )
        )
      },
      fastForwardToCYAEnabled = false
    )

  private def validateDepositIdAndTaxCode(journey: SecuritiesJourney, securityDepositId: String, taxCode: TaxCode)(
    implicit request: Request[_]
  ): Future[Either[Result, (Option[BigDecimal], BigDecimal)]] = {
    val reclaimsForDepositId: Option[SecuritiesJourney.CorrectedAmounts] =
      journey.answers.correctedAmounts.flatMap(_.get(securityDepositId))

    (reclaimsForDepositId match {
      case None =>
        if (journey.getSecurityDepositIds.contains(securityDepositId))
          Left(Redirect(routes.ConfirmFullRepaymentController.show(securityDepositId)))
        else
          Left(
            logAndDisplayError(
              s"Invalid depositId=$securityDepositId. Available deposit IDs",
              journey.getSecurityDepositIds.mkString(",")
            )
          )

      case Some(reclaims) =>
        reclaims.get(taxCode) match {
          case None =>
            Left(Redirect(routes.SelectDutiesController.show(securityDepositId)))

          case Some(reclaimAmountOpt) =>
            val totalAmountOnDeclaration =
              journey.getSecurityTaxDetailsFor(securityDepositId, taxCode).map(_.getAmount)

            totalAmountOnDeclaration match {
              case None =>
                Left(
                  logAndDisplayError(
                    s"Cannot find the amount of a taxType=$taxCode paid for a depositId=$securityDepositId. Available tax codes",
                    journey.getSecurityTaxCodesFor(securityDepositId).mkString(",")
                  )
                )

              case Some(totalAmount) =>
                Right((reclaimAmountOpt, totalAmount))
            }

        }
    }).asFuture
  }

  private def nextPage(
    journey: SecuritiesJourney,
    securityDepositId: String,
    taxCode: TaxCode,
    amountHasChanged: Boolean
  ): Call =
    if (journey.answers.modes.checkClaimDetailsChangeMode && journey.answers.modes.claimFullAmountMode) {
      if (journey.userHasSeenCYAPage && !amountHasChanged)
        routes.CheckYourAnswersController.show
      else
        journey.getNextDepositIdAndTaxCodeToClaim match {
          case Some(Left(depositId)) =>
            routes.ConfirmFullRepaymentController.show(depositId)

          case Some(Right((depositId, tc))) =>
            routes.EnterClaimController.show(depositId, tc)

          case None =>
            routes.CheckClaimDetailsController.show
        }
    } else
      journey
        .getSelectedDutiesFor(securityDepositId)
        .flatMap(_.nextAfter(taxCode)) match {

        case Some(nextTaxCode) =>
          routes.EnterClaimController.show(securityDepositId, nextTaxCode)

        case None =>
          journey.getSelectedDepositIds.nextAfter(securityDepositId) match {
            case Some(nextSecurityDepositId) =>
              if (journey.answers.modes.checkClaimDetailsChangeMode && !journey.answers.modes.claimFullAmountMode)
                routes.CheckClaimDetailsController.show
              else
                routes.ConfirmFullRepaymentController.show(nextSecurityDepositId)

            case None =>
              routes.CheckClaimDetailsController.show
          }
      }

}
