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

import cats.data.EitherT
import com.github.arturopala.validator.Validator.Validate
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.DeclarationConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.GetXiEoriMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.CommunitySystemsOfDutyRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.EndUseRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.OutwardProcessingRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.GetDeclarationError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.choose_reason_for_security
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseReasonForSecurityController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  DeclarationConnector: DeclarationConnector,
  val xiEoriConnector: XiEoriConnector,
  featureSwitchService: FeatureSwitchService,
  chooseReasonForSecurityPage: choose_reason_for_security
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends SecuritiesJourneyBaseController
    with GetXiEoriMixin {

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(SecuritiesJourney.Checks.hasMovementReferenceNumber)

  private val postAction: Call = routes.ChooseReasonForSecurityController.submit

  // Success: Declaration has been found and claim for this MRN and RfS does not exist yet.
  private val successResultSelectSecurities: Result =
    Redirect(routes.SelectSecuritiesController.showFirst())

  // Success: Declaration has been found and claim for this MRN and RfS does not exist yet.
  private val successResultEnterImporterEori: Result =
    Redirect(routes.EnterImporterEoriNumberController.show)

  // Error: Claim has already been submitted as part of a whole or partial claim
  private val errorResultClaimExistsAlready: Result =
    Redirect(routes.ClaimInvalidTPI04Controller.show)

  private def ntasOptions(implicit hc: HeaderCarrier): Set[ReasonForSecurity] =
    if featureSwitchService.isEnabled(Feature.SecurityReasonsNtas) then ReasonForSecurity.ntas else Set.empty
  private def niruOptions(implicit hc: HeaderCarrier): Set[ReasonForSecurity] =
    if featureSwitchService.isEnabled(Feature.SecurityReasonsNiru) then
      filterDisabledNiruOptions(ReasonForSecurity.niru)
    else Set.empty

  private def filterDisabledNiruOptions(
    options: Set[ReasonForSecurity]
  )(implicit hc: HeaderCarrier): Set[ReasonForSecurity] =
    options
      .filterNot(rfs =>
        if featureSwitchService.isDisabled(Feature.SecurityReasonsNiruOpr) then { rfs == OutwardProcessingRelief }
        else false
      )
      .filterNot(rfs =>
        if featureSwitchService.isDisabled(Feature.SecurityReasonsNiruCsdr) then { rfs == CommunitySystemsOfDutyRelief }
        else false
      )

  private def nidacOptions(implicit hc: HeaderCarrier): Set[ReasonForSecurity] =
    if featureSwitchService.isEnabled(Feature.SecurityReasonsNidac) then ReasonForSecurity.nidac else Set.empty

  private def reasonsForSecurity(implicit hc: HeaderCarrier): Set[ReasonForSecurity] =
    ntasOptions ++ niruOptions ++ nidacOptions

  private val form: Form[ReasonForSecurity] = Forms.reasonForSecurityForm

  override def isXiEoriSupported(implicit hc: HeaderCarrier): Boolean =
    featureSwitchService.isEnabled(Feature.XiEori)

  override def modifyJourney(journey: Journey, userXiEori: UserXiEori): Journey =
    journey.submitUserXiEori(userXiEori)

  def show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val reasonForSecurityForm: Form[ReasonForSecurity] =
      Forms.reasonForSecurityForm.withDefault(journey.getReasonForSecurity)
    Ok(
      chooseReasonForSecurityPage(reasonForSecurityForm, reasonsForSecurity, postAction)
    ).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(chooseReasonForSecurityPage(formWithErrors, reasonsForSecurity, postAction))
          ).asFuture,
        reasonForSecurity =>
          if journey.getReasonForSecurity.contains(reasonForSecurity) then
            (
              journey,
              if journey.answers.modes.checkDeclarationDetailsChangeMode
              then Redirect(routes.CheckDeclarationDetailsController.show)
              else if journey.reasonForSecurityIsIPR
              then Redirect(routes.CheckDeclarationDetailsWithoutSecuritiesSelectionController.show)
              else if journey.reasonForSecurityIsEndUseRelief
              then Redirect(routes.CheckTotalImportDischargedController.show)
              else successResultSelectSecurities
            ).asFuture
          else
            (for
              mrn                          <- getMovementReferenceNumber(journey)
              declaration                  <- lookupDisplayDeclaration(mrn, reasonForSecurity)
              _                            <- checkIfDeclarationHaveSecurityDeposits(declaration)
              updatedDeclaration            =
                reasonForSecurity match {
                  case EndUseRelief =>
                    val updatedSecurityDetails =
                      declaration.displayResponseDetail.securityDetails.map { securityDetails =>
                        securityDetails.map(sd =>
                          sd.copy(taxDetails =
                            sd.taxDetails.filterNot(td => TaxCodes.vatTaxCodes.contains(td.getTaxCode))
                          )
                        )
                      }
                    declaration.copy(
                      displayResponseDetail = declaration.displayResponseDetail.copy(
                        securityDetails = updatedSecurityDetails
                      )
                    )
                  case _            => declaration
                }
              updatedJourney               <- submitReasonForSecurityAndDeclaration(journey, reasonForSecurity, updatedDeclaration)
              journeyWithRfsAndDeclaration <- tryGetUserXiEoriIfNeeded(updatedJourney)
              updatedJourneyWithRedirect   <-
                if SecuritiesJourney.Checks
                    .declarantOrImporterEoriMatchesUserOrHasBeenVerified(journeyWithRfsAndDeclaration)
                    .isInvalid
                then redirectToEnterImporterEoriNumber(journeyWithRfsAndDeclaration)
                else
                  for
                    similarClaimExistAlreadyInCDFPay <- checkIfClaimIsDuplicated(mrn, reasonForSecurity)
                    updatedJourneyWithRedirect       <- submitClaimDuplicateCheckStatus(
                                                          journeyWithRfsAndDeclaration,
                                                          similarClaimExistAlreadyInCDFPay
                                                        )
                  yield updatedJourneyWithRedirect
            yield updatedJourneyWithRedirect)
              .bimap(result => (journey, result), identity)
              .merge
      )
  }

  private def tryGetUserXiEoriIfNeeded(journey: SecuritiesJourney)(implicit
    hc: HeaderCarrier,
    r: Request[?]
  ): EitherT[Future, Result, SecuritiesJourney] =
    getUserXiEoriIfNeeded(journey, enabled = true)
      .leftMap(error => logAndDisplayError("Could not get XI EORI", error))

  private def getMovementReferenceNumber(journey: SecuritiesJourney): EitherT[Future, Result, MRN] =
    EitherT.fromOption[Future](
      journey.getLeadMovementReferenceNumber,
      Redirect(routes.EnterMovementReferenceNumberController.show)
    )

  private def lookupDisplayDeclaration(mrn: MRN, reasonForSecurity: ReasonForSecurity)(implicit
    errorHandler: ErrorHandler,
    hc: HeaderCarrier,
    r: Request[?]
  ): EitherT[Future, Result, DisplayDeclaration] =
    claimService
      .getDisplayDeclarationWithErrorCodes(mrn, reasonForSecurity)
      .leftMap {
        case GetDeclarationError.declarationNotFound      => Redirect(routes.DeclarationNotFoundController.show)
        case GetDeclarationError.invalidReasonForSecurity => Redirect(routes.InvalidReasonForSecurityController.show)
        case _                                            => errorHandler.errorResult()
      }

  private def checkIfDeclarationHaveSecurityDeposits(declaration: DisplayDeclaration): EitherT[Future, Result, String] =
    EitherT.fromOption[Future](
      declaration.getSecurityDepositIds.flatMap(_.headOption),
      Redirect(routes.ChooseReasonForSecurityController.show)
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
    r: Request[?]
  ): EitherT[Future, Result, Boolean] =
    DeclarationConnector
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
            if similarClaimExistAlreadyInCDFPay then {
              logger.info("Claim ineligible because already exists.")
              errorResultClaimExistsAlready
            } else if journeyWithUpdatedStatus.reasonForSecurityIsIPR then {
              Redirect(routes.CheckDeclarationDetailsWithoutSecuritiesSelectionController.show)
            } else if journeyWithUpdatedStatus.reasonForSecurityIsEndUseRelief then {
              Redirect(routes.CheckTotalImportDischargedController.show)
            } else successResultSelectSecurities
          )
        )
        .merge
    )

}
