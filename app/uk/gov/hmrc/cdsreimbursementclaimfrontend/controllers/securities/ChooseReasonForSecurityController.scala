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

import cats.data.EitherT
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.CDSReimbursementClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.choose_reason_for_security
import uk.gov.hmrc.http.HeaderCarrier
import com.github.arturopala.validator.Validator.Validate

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseReasonForSecurityController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  cdsReimbursementClaimConnector: CDSReimbursementClaimConnector,
  chooseReasonForSecurityPage: choose_reason_for_security
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends SecuritiesJourneyBaseController {

  private val postAction: Call = routes.ChooseReasonForSecurityController.submit()

  //Success: Declaration has been found and claim for this MRN and RfS does not exist yet.
  private val successResultSelectSecurities: Result =
    Redirect(routes.SelectSecuritiesController.showFirst())

  //Success: Declaration has been found and ReasonForSecurity is InwardProcessingRelief.
  private val successResultBOD3: Result =
    Redirect(routes.BillOfDischargeController.showBOD3())

  //Success: Declaration has been found and ReasonForSecurity is EndUseRelief.
  private val successResultBOD4: Result =
    Redirect(routes.BillOfDischargeController.showBOD4())

  //Success: Declaration has been found and claim for this MRN and RfS does not exist yet.
  private val successResultEnterImporterEori: Result =
    Redirect(routes.EnterImporterEoriNumberController.show())

  //Error: Claim has already been submitted as part of a whole or partial claim
  private val errorResultClaimExistsAlready: Result =
    Redirect(routes.ClaimInvalidTPI04Controller.show())

  //Error: Security chosen does not exist against the Movement Reference Number (MRN) provided
  private val errorResultDeclarationNotFoundForRfS: Result =
    Redirect(routes.ChooseReasonForSecurityController.show()) // TODO: fix in CDSR-1773

  //Error: Movement Reference Number (MRN) provided does not exist
  private val errorResultDeclarationNotFoundForMrn: Result =
    errorResultDeclarationNotFoundForRfS // TODO: fix in CDSR-1773

  private val reasonsForSecurity: Set[ReasonForSecurity] = ReasonForSecurity.values

  private val form: Form[ReasonForSecurity] = Forms.reasonForSecurityForm

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(SecuritiesJourney.Checks.hasMovementReferenceNumber)

  def show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val reasonForSecurityForm: Form[ReasonForSecurity] =
      Forms.reasonForSecurityForm.withDefault(journey.getReasonForSecurity)
    Ok(
      chooseReasonForSecurityPage(reasonForSecurityForm, reasonsForSecurity, postAction)
    ).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    form.bindFromRequest
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(chooseReasonForSecurityPage(formWithErrors, reasonsForSecurity, postAction))
          ).asFuture,
        reasonForSecurity =>
          if (journey.getReasonForSecurity.contains(reasonForSecurity))
            (
              journey,
              if (journey.answers.checkDeclarationDetailsChangeMode)
                Redirect(routes.CheckDeclarationDetailsController.show())
              else
                successResultSelectSecurities
            ).asFuture
          else
            (for {
              mrn                          <- getMovementReferenceNumber(journey)
              declaration                  <- lookupDisplayDeclaration(mrn, reasonForSecurity)
              _                            <- checkIfDeclarationHaveSecurityDeposits(declaration)
              journeyWithRfsAndDeclaration <-
                submitReasonForSecurityAndDeclaration(journey, reasonForSecurity, declaration)
              updatedJourneyWithRedirect   <-
                if (
                  SecuritiesJourney.Checks
                    .declarantOrImporterEoriMatchesUserOrHasBeenVerified(journeyWithRfsAndDeclaration)
                    .isInvalid
                )
                  redirectToEnterImporterEoriNumber(journeyWithRfsAndDeclaration)
                else
                  for {
                    similarClaimExistAlreadyInCDFPay <- checkIfClaimIsDuplicated(mrn, reasonForSecurity)
                    updatedJourneyWithRedirect       <- submitClaimDuplicateCheckStatus(
                                                          journeyWithRfsAndDeclaration,
                                                          similarClaimExistAlreadyInCDFPay
                                                        )
                  } yield updatedJourneyWithRedirect
            } yield updatedJourneyWithRedirect)
              .bimap(result => (journey, result), identity)
              .merge
      )
  }

  private def getMovementReferenceNumber(journey: SecuritiesJourney): EitherT[Future, Result, MRN] =
    EitherT.fromOption[Future](
      journey.getLeadMovementReferenceNumber,
      Redirect(routes.EnterMovementReferenceNumberController.show())
    )

  private def lookupDisplayDeclaration(mrn: MRN, reasonForSecurity: ReasonForSecurity)(implicit
    errorHandler: ErrorHandler,
    hc: HeaderCarrier,
    r: Request[_]
  ): EitherT[Future, Result, DisplayDeclaration] =
    claimService
      .getDisplayDeclaration(mrn, reasonForSecurity)
      .leftMap(error => logAndDisplayError("Could not get the declaration", error))
      .flatMap {
        case None              =>
          EitherT.leftT[Future, DisplayDeclaration](
            errorResultDeclarationNotFoundForRfS
          )
        case Some(declaration) =>
          EitherT.rightT[Future, Result](declaration)
      }

  private def checkIfDeclarationHaveSecurityDeposits(declaration: DisplayDeclaration): EitherT[Future, Result, String] =
    EitherT.fromOption[Future](
      declaration.getSecurityDepositIds.flatMap(_.headOption),
      Redirect(routes.ChooseReasonForSecurityController.show())
    )

  private def submitReasonForSecurityAndDeclaration(
    journey: SecuritiesJourney,
    reasonForSecurity: ReasonForSecurity,
    declaration: DisplayDeclaration
  ): EitherT[Future, Result, SecuritiesJourney] =
    EitherT
      .fromEither[Future](
        journey
          .submitReasonForSecurityAndDeclaration(reasonForSecurity, declaration)
      )
      .leftMap(error => Redirect(routeForValidationError(error)))

  private def redirectToEnterImporterEoriNumber(journey: SecuritiesJourney) =
    EitherT.rightT[Future, Result](
      (
        journey,
        successResultEnterImporterEori
      )
    )

  private def checkIfClaimIsDuplicated(mrn: MRN, reasonForSecurity: ReasonForSecurity)(implicit
    hc: HeaderCarrier,
    r: Request[_]
  ): EitherT[Future, Result, Boolean] =
    cdsReimbursementClaimConnector
      .getIsDuplicate(mrn, reasonForSecurity)
      .leftMap(error => logAndDisplayError("Could not check if isDuplicate claim", error))
      .map(_.claimFound)

  private def submitClaimDuplicateCheckStatus(
    journey: SecuritiesJourney,
    similarClaimExistAlreadyInCDFPay: Boolean
  ): EitherT[Future, Result, (SecuritiesJourney, Result)] =
    EitherT.liftF[Future, Result, (SecuritiesJourney, Result)](
      EitherT
        .fromEither[Future](
          journey
            .submitClaimDuplicateCheckStatus(similarClaimExistAlreadyInCDFPay)
        )
        .leftMap(error =>
          (
            journey,
            Redirect(routeForValidationError(error))
          )
        )
        .map(journeyWithUpdatedStatus =>
          (
            journeyWithUpdatedStatus,
            if (similarClaimExistAlreadyInCDFPay) {
              logger.info(s"Claim ineligible because already exists.")
              errorResultClaimExistsAlready
            } else if (journeyWithUpdatedStatus.reasonForSecurityIsIPR) {
              successResultBOD3
            } else if (journeyWithUpdatedStatus.reasonForSecurityIsEndUseRelief) {
              successResultBOD4
            } else
              successResultSelectSecurities
          )
        )
        .merge
    )

}
