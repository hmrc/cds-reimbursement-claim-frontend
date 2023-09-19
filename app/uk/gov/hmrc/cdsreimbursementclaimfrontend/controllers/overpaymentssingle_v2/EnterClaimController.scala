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
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks.hasMRNAndDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.enter_single_claim

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  enterClaim: enter_single_claim
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsSingleJourneyBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[Journey]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val key: String                 = "enter-claim"
  final val postAction: TaxCode => Call = routes.EnterClaimController.submit

  final val showFirst: Action[AnyContent] =
    simpleActionReadJourney(journey =>
      journey.getSelectedDuties.flatMap(_.headOption) match {
        case None =>
          Redirect(routes.SelectDutiesController.show)

        case Some(taxCode) =>
          Redirect(routes.EnterClaimController.show(taxCode))
      }
    )

  final def show(taxCode: TaxCode): Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      val isSubsidyOnly: Boolean = journey.isSubsidyOnlyJourney
      journey.getSelectedDuties match {
        case None =>
          Redirect(routes.SelectDutiesController.show).asFuture

        case Some(selectedDuties) if selectedDuties.contains(taxCode) =>
          journey.getNdrcDetailsFor(taxCode) match {
            case None =>
              redirectWhenInvalidTaxCode(journey).asFuture

            case Some(ndrcDetails) =>
              val actualAmount: Option[BigDecimal] =
                journey.answers.correctedAmounts.flatMap(_.get(taxCode).flatten)
              val amountPaid                       =
                BigDecimal(ndrcDetails.amount)
              val form                             =
                Forms.actualAmountForm(key, amountPaid).withDefault(actualAmount)
              Ok(
                enterClaim(
                  form,
                  TaxCode(ndrcDetails.taxType),
                  amountPaid,
                  isSubsidyOnly,
                  postAction(taxCode)
                )
              ).asFuture
          }

        case _ =>
          redirectWhenInvalidTaxCode(journey).asFuture
      }
    }

  final def submit(taxCode: TaxCode): Action[AnyContent] =
    actionReadWriteJourney(
      { implicit request => journey =>
        val isSubsidyOnly: Boolean = journey.isSubsidyOnlyJourney
        journey.getSelectedDuties match {
          case None =>
            (journey, Redirect(routes.SelectDutiesController.show)).asFuture

          case Some(selectedDuties) if selectedDuties.contains(taxCode) =>
            journey.getNdrcDetailsFor(taxCode) match {
              case Some(ndrcDetails) =>
                Forms
                  .actualAmountForm(key, BigDecimal(ndrcDetails.amount))
                  .bindFromRequest()
                  .fold(
                    formWithErrors =>
                      Future.successful(
                        (
                          journey,
                          BadRequest(
                            enterClaim(
                              formWithErrors,
                              TaxCode(ndrcDetails.taxType),
                              BigDecimal(ndrcDetails.amount),
                              isSubsidyOnly,
                              postAction(taxCode)
                            )
                          )
                        )
                      ),
                    reimbursementAmount =>
                      journey
                        .submitCorrectAmount(taxCode, reimbursementAmount)
                        .fold(
                          error =>
                            Future.failed(new Exception(s"Cannot submit amount for $taxCode reimbursement - $error")),
                          updatedJourney =>
                            (
                              updatedJourney,
                              redirectToNextPage(updatedJourney, taxCode)
                            ).asFuture
                        )
                  )

              case None =>
                logger.error("Attempting to claim a reimbursement before selecting an MRN")
                Future.successful((journey, Redirect(routes.EnterMovementReferenceNumberController.show)))
            }

          case _ =>
            (journey, redirectWhenInvalidTaxCode(journey)).asFuture
        }
      },
      fastForwardToCYAEnabled = false
    )

  private def redirectWhenInvalidTaxCode(journey: Journey): Result =
    Redirect {
      if (journey.hasCompleteReimbursementClaims)
        routes.CheckClaimDetailsController.show
      else
        routes.SelectDutiesController.show
    }

  private def redirectToNextPage(journey: Journey, taxCode: TaxCode): Result =
    Redirect {
      if (journey.hasCompleteReimbursementClaims && !journey.answers.dutiesChangeMode)
        routes.CheckClaimDetailsController.show
      else {
        val selectedTaxCodes = journey.getSelectedDuties.getOrElse(Seq.empty)
        selectedTaxCodes.indexOf(taxCode) match {
          case -1 => // invalid tax code
            routes.SelectDutiesController.show

          case n if n < selectedTaxCodes.size - 1 =>
            routes.EnterClaimController.show(selectedTaxCodes(n + 1))

          case _ =>
            routes.CheckClaimDetailsController.show
        }
      }
    }

}
