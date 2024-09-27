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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AuditService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.OverpaymentsSingleClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.confirmation_of_submission
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.submit_claim_error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.check_your_answers_single
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.check_your_answers_single_pdf
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyLog

import scala.concurrent.ExecutionContext
import _root_.com.hhandoko.play.pdf.PdfGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature.ShowPdfDownloadOption
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.CheckYourAnswersPdfHelper.getPdfUrl

@Singleton
class CheckYourAnswersController @Inject() (
  val jcc: JourneyControllerComponents,
  overpaymentsSingleClaimConnector: OverpaymentsSingleClaimConnector,
  uploadDocumentsConnector: UploadDocumentsConnector,
  checkYourAnswersPage: check_your_answers_single,
  checkYourAnswersPagePdf: check_your_answers_single_pdf,
  confirmationOfSubmissionPage: confirmation_of_submission,
  submitClaimFailedPage: submit_claim_error,
  auditService: AuditService,
  pdfGenerator: PdfGenerator,
  featureSwitchService: FeatureSwitchService
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsSingleJourneyBaseController {

  private val postAction: Call             = routes.CheckYourAnswersController.submit
  private val showConfirmationAction: Call = routes.CheckYourAnswersController.showConfirmation

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val selfUrl: String = jcc.servicesConfig.getString("self.url")

  final val show: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      journey
        .submitCheckYourAnswersChangeMode(true)
        .toOutput
        .fold(
          errors => {
            logger.warn(s"Claim not ready to show the CYA page because of ${errors.mkString(",")}")
            (
              journey.submitCheckYourAnswersChangeMode(false),
              Redirect(routeForValidationErrors(errors))
            )
          },
          output =>
            (
              journey.submitCheckYourAnswersChangeMode(true),
              Ok(
                checkYourAnswersPage(
                  output,
                  journey.isAllSelectedDutiesAreCMAEligible,
                  journey.isSubsidyOnlyJourney,
                  journey.answers.displayDeclaration,
                  postAction,
                  showPdfOption = featureSwitchService.isEnabled(ShowPdfDownloadOption)
                )
              )
            )
        )
        .asFuture
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      if (journey.isFinalized)
        (journey, Redirect(showConfirmationAction)).asFuture
      else
        journey
          .submitCheckYourAnswersChangeMode(true)
          .toOutput
          .fold(
            errors => {
              logger.warn(s"Claim not ready to submit because of ${errors.mkString(",")}")
              (journey, Redirect(routeForValidationErrors(errors))).asFuture
            },
            output =>
              overpaymentsSingleClaimConnector
                .submitClaim(OverpaymentsSingleClaimConnector.Request(output))
                .flatMap { response =>
                  logger.info(
                    s"Successful submit of claim for ${output.movementReferenceNumber} with case number ${response.caseNumber}."
                  )
                  val summary =
                    JourneyLog(output, journey.answers.userEoriNumber.value, Some(response.caseNumber), journey)
                      .logInfo()
                  auditService.sendSuccessfulClaimEvent(journey, output, summary)
                  uploadDocumentsConnector.wipeOut
                    .map(_ =>
                      (
                        journey.finalizeJourneyWith(response.caseNumber).getOrElse(journey),
                        Redirect(showConfirmationAction)
                      )
                    )
                }
                .recover { case e =>
                  logger.error(s"Failed to submit claim for ${output.movementReferenceNumber} because of $e.")
                  val summary = JourneyLog(output, journey.answers.userEoriNumber.value, None, journey).logError(e)
                  auditService.sendFailedClaimEvent(journey, output, summary)
                  (journey, Ok(submitClaimFailedPage()))
                }
          )
    }

  final val showConfirmation: Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(getJourney)
          .map { journey =>
            val maybeMrn   = journey.getLeadMovementReferenceNumber.map(_.value)
            val maybeEmail = journey.answers.contactDetails.flatMap(_.emailAddress).map(_.value)
            (journey.caseNumber match {

              case Some(caseNumber) =>
                Ok(
                  confirmationOfSubmissionPage(
                    journey.getTotalReimbursementAmount,
                    caseNumber,
                    maybeMrn = maybeMrn,
                    maybeEmail = maybeEmail,
                    subKey = Some("single"),
                    featureSwitchService.isEnabled(ShowPdfDownloadOption),
                    pdfUrl = getPdfUrl(journey)
                  )
                )
              case None             => Redirect(checkYourAnswers)
            }).asFuture
          }
          .getOrElse(redirectToTheStartOfTheJourney)
      }

  final val showPdf: Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(getJourney)
          .map { journey =>
            journey.toOutput.fold(
              errors => {
                logger.warn(s"Claim not ready to show the CYA page because of ${errors.mkString(",")}")
                Redirect(routeForValidationErrors(errors)).asFuture
              },
              output =>
                journey.caseNumber match {
                  case Some(caseNumber) =>
                    pdfGenerator
                      .ok(
                        checkYourAnswersPagePdf(caseNumber, output, journey.answers.displayDeclaration),
                        selfUrl
                      )
                      .asFuture
                  case None             => Redirect(checkYourAnswers).asFuture
                }
            )

          }
          .getOrElse(redirectToTheStartOfTheJourney)
      }
}
