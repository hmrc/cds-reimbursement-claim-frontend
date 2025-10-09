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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.EndUseRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.GetDeclarationError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.choose_reason_for_security
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseReasonForSecurityController @Inject() (
  val jcc: ClaimControllerComponents,
  claimService: ClaimService,
  DeclarationConnector: DeclarationConnector,
  val xiEoriConnector: XiEoriConnector,
  chooseReasonForSecurityPage: choose_reason_for_security
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends SecuritiesClaimBaseController
    with GetXiEoriMixin {

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place.
  override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(SecuritiesClaim.Checks.hasMovementReferenceNumber)

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

  private def reasonsForSecurity(claim: SecuritiesClaim): Set[ReasonForSecurity] = claim.features match
    case Some(features) => features.availableReasonsForSecurity
    case None           => Set.empty

  private val form: Form[ReasonForSecurity] = Forms.reasonForSecurityForm

  override def modifyClaim(claim: Claim, userXiEori: UserXiEori): Claim =
    claim.submitUserXiEori(userXiEori)

  def show: Action[AnyContent] = actionReadClaim { implicit request => claim =>

    val reasonForSecurityForm: Form[ReasonForSecurity] =
      Forms.reasonForSecurityForm.withDefault(claim.getReasonForSecurity)

    Ok(
      chooseReasonForSecurityPage(reasonForSecurityForm, reasonsForSecurity(claim), postAction)
    )
  }

  val submit: Action[AnyContent] = actionReadWriteClaim { implicit request => claim =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            claim,
            BadRequest(chooseReasonForSecurityPage(formWithErrors, reasonsForSecurity(claim), postAction))
          ),
        reasonForSecurity =>
          if claim.getReasonForSecurity.contains(reasonForSecurity) then
            (
              claim,
              if claim.answers.modes.checkDeclarationDetailsChangeMode
              then Redirect(routes.CheckDeclarationDetailsController.show)
              else if claim.reasonForSecurityIsIPROrENU
              then Redirect(routes.CheckDeclarationDetailsWithoutSecuritiesSelectionController.show)
              else successResultSelectSecurities
            )
          else
            (for
              mrn                        <- getMovementReferenceNumber(claim)
              declaration                <- lookupDisplayDeclaration(mrn, reasonForSecurity)
              _                          <- checkIfDeclarationHaveSecurityDeposits(declaration)
              updatedDeclaration          =
                reasonForSecurity match {
                  case EndUseRelief =>
                    val updatedSecurityDetails =
                      declaration.displayResponseDetail.securityDetails.map { securityDetails =>
                        securityDetails.map(sd => sd.copy(taxDetails = sd.taxDetails.filterNot(_.getTaxCode.isVAT)))
                      }
                    declaration.copy(
                      displayResponseDetail = declaration.displayResponseDetail.copy(
                        securityDetails = updatedSecurityDetails
                      )
                    )
                  case _            => declaration
                }
              updatedClaim               <- submitReasonForSecurityAndDeclaration(claim, reasonForSecurity, updatedDeclaration)
              claimWithRfsAndDeclaration <- tryGetUserXiEoriIfNeeded(updatedClaim)
              updatedClaimWithRedirect   <-
                if SecuritiesClaim.Checks
                    .declarantOrImporterEoriMatchesUserOrHasBeenVerified(claimWithRfsAndDeclaration)
                    .isInvalid
                then redirectToEnterImporterEoriNumber(claimWithRfsAndDeclaration)
                else
                  for
                    similarClaimExistAlreadyInCDFPay <- checkIfClaimIsDuplicated(mrn, reasonForSecurity)
                    updatedClaimWithRedirect         <- submitClaimDuplicateCheckStatus(
                                                          claimWithRfsAndDeclaration,
                                                          similarClaimExistAlreadyInCDFPay
                                                        )
                  yield updatedClaimWithRedirect
            yield updatedClaimWithRedirect)
              .bimap(result => (claim, result), identity)
              .merge
      )
  }

  private def tryGetUserXiEoriIfNeeded(claim: SecuritiesClaim)(implicit
    hc: HeaderCarrier,
    r: Request[?]
  ): EitherT[Future, Result, SecuritiesClaim] =
    getUserXiEoriIfNeeded(claim, enabled = true)
      .leftMap(error => logAndDisplayError("Could not get XI EORI", error))

  private def getMovementReferenceNumber(claim: SecuritiesClaim): EitherT[Future, Result, MRN] =
    EitherT.fromOption[Future](
      claim.getLeadMovementReferenceNumber,
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
    claim: SecuritiesClaim,
    reasonForSecurity: ReasonForSecurity,
    declaration: DisplayDeclaration
  ): EitherT[Future, Result, SecuritiesClaim] =
    EitherT
      .fromEither[Future](
        claim
          .submitReasonForSecurityAndDeclaration(reasonForSecurity, declaration)
      )
      .leftMap(error => Redirect(routeForValidationError(error)))

  private def redirectToEnterImporterEoriNumber(claim: SecuritiesClaim) =
    EitherT.rightT[Future, Result](
      (
        claim,
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
    claim: SecuritiesClaim,
    similarClaimExistAlreadyInCDFPay: Boolean
  ): EitherT[Future, Result, (SecuritiesClaim, Result)] =
    EitherT.liftF[Future, Result, (SecuritiesClaim, Result)](
      EitherT
        .fromEither[Future](
          claim
            .submitClaimDuplicateCheckStatus(similarClaimExistAlreadyInCDFPay)
        )
        .leftMap(error =>
          (
            claim,
            Redirect(routeForValidationError(error))
          )
        )
        .map(claimWithUpdatedStatus =>
          (
            claimWithUpdatedStatus,
            if similarClaimExistAlreadyInCDFPay then {
              logger.info("Claim ineligible because already exists.")
              errorResultClaimExistsAlready
            } else if claimWithUpdatedStatus.reasonForSecurityIsIPROrENU then {
              Redirect(routes.CheckDeclarationDetailsWithoutSecuritiesSelectionController.show)
            } else successResultSelectSecurities
          )
        )
        .merge
    )

}
