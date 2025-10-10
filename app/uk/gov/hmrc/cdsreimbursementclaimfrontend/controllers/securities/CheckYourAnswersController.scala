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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.SecuritiesClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyLog
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AuditService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.CheckYourAnswersPrintViewHelper.getPrintViewUrl
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.submit_claim_error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_your_answers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_your_answers_print_view
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.confirmation_of_submission

import scala.concurrent.ExecutionContext

@Singleton
class CheckYourAnswersController @Inject() (
  val jcc: ClaimControllerComponents,
  securitiesClaimConnector: SecuritiesClaimConnector,
  uploadDocumentsConnector: UploadDocumentsConnector,
  checkYourAnswersPage: check_your_answers,
  checkYourAnswersPagePrintView: check_your_answers_print_view,
  confirmationOfSubmissionPage: confirmation_of_submission,
  submitClaimFailedPage: submit_claim_error,
  auditService: AuditService
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends SecuritiesClaimBaseController
    with Logging {

  private val postAction: Call             = routes.CheckYourAnswersController.submit
  private val showConfirmationAction: Call = routes.CheckYourAnswersController.showConfirmation

  val show: Action[AnyContent] =
    actionReadWriteClaim { implicit request => claim =>
      claim
        .submitCheckYourAnswersChangeMode(true)
        .toOutput
        .fold(
          errors => {
            logger.warn(s"Claim not ready to show the CYA page because of ${errors.mkString(",")}")
            (
              claim.submitCheckYourAnswersChangeMode(false),
              Redirect(routeForValidationErrors(errors))
            )
          },
          output =>
            (
              claim.submitCheckYourAnswersChangeMode(true),
              Ok(
                checkYourAnswersPage(
                  output,
                  claim.isSingleSecurity,
                  claim.answers.displayDeclaration,
                  claim.answers.exportMovementReferenceNumbers,
                  postAction
                )
              )
            )
        )
    }

  val submit: Action[AnyContent] =
    actionReadWriteClaim { implicit request => claim =>
      claim
        .submitCheckYourAnswersChangeMode(true)
        .toOutput
        .fold(
          errors => {
            logger.warn(s"Claim not ready to submit because of ${errors.mkString(",")}")
            (claim, Redirect(routeForValidationErrors(errors))).asFuture
          },
          output =>
            securitiesClaimConnector
              .submitClaim(SecuritiesClaimConnector.Request(output), mitigate403 = true)
              .flatMap { response =>
                logger.info(
                  s"Successful submit of claim for ${output.movementReferenceNumber} with case number ${response.caseNumber}."
                )
                val summary =
                  JourneyLog(output, claim.answers.userEoriNumber.value, Some(response.caseNumber), claim)
                    .logInfo()
                auditService.sendSuccessfulClaimEvent(claim, output, summary)
                uploadDocumentsConnector.wipeOut
                  .map(_ =>
                    (
                      claim.finalizeClaimWith(response.caseNumber).getOrElse(claim),
                      Redirect(showConfirmationAction)
                    )
                  )
              }
              .recover { case e =>
                logger.error(s"Failed to submit claim for ${output.movementReferenceNumber} because of $e.")
                val summary = JourneyLog(output, claim.answers.userEoriNumber.value, None, claim).logError(e)
                auditService.sendFailedClaimEvent(claim, output, summary)
                (claim, Ok(submitClaimFailedPage()))
              }
        )
    }

  val showConfirmation: Action[AnyContent] =
    jcc.authenticatedActionWithSessionData
      .async { implicit request =>
        request.sessionData
          .flatMap(getClaim)
          .map { claim =>
            val maybeMrn   = claim.getLeadMovementReferenceNumber.map(_.value)
            val maybeEmail = claim.answers.contactDetails.flatMap(_.emailAddress).map(_.value)
            (claim.caseNumber match {

              case Some(caseNumber) =>
                Ok(
                  confirmationOfSubmissionPage(
                    claim.getTotalClaimAmount,
                    caseNumber,
                    maybeMrn = maybeMrn,
                    maybeEmail = maybeEmail,
                    subKey = Some("securities"),
                    printViewUrl = getPrintViewUrl(claim)
                  )
                )
              case None             => Redirect(checkYourAnswers)
            }).asFuture
          }
          .getOrElse(redirectToTheStartOfTheClaim)
      }

  val showPrintView: Action[AnyContent] =
    jcc.authenticatedActionWithSessionData
      .async { implicit request =>
        request.sessionData
          .flatMap(getClaim)
          .map(claim =>
            claim.toOutput.fold(
              errors => {
                logger.warn(s"Claim not ready to show the CYA page because of ${errors.mkString(",")}")
                Redirect(routeForValidationErrors(errors)).asFuture
              },
              output =>
                (claim.caseNumber, claim.submissionDateTime) match {
                  case (Some(caseNumber), Some(submissionDate)) =>
                    Ok(
                      checkYourAnswersPagePrintView(
                        caseNumber,
                        claim.getTotalClaimAmount,
                        output,
                        claim.isSingleSecurity,
                        claim.answers.displayDeclaration,
                        claim.answers.exportMovementReferenceNumbers,
                        submissionDate
                      )
                    ).asFuture
                  case _                                        =>
                    logger.warn("Error fetching claim for print view")
                    errorHandler.errorResult().asFuture
                }
            )
          )
          .getOrElse(redirectToTheStartOfTheClaim)
      }
}
