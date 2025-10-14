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
import play.api.data.FormError
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.DeclarationConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.eoriNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_declarant_eori_number
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.language.postfixOps

@Singleton
class EnterDeclarantEoriNumberController @Inject() (
  val jcc: ClaimControllerComponents,
  DeclarationConnector: DeclarationConnector,
  enterDeclarantEoriNumberPage: enter_declarant_eori_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends SecuritiesClaimBaseController {

  val formKey: String  = "enter-declarant-eori-number"
  val postAction: Call = routes.EnterDeclarantEoriNumberController.submit

  // Success: Declaration has been found and claim for this MRN and RfS does not exist yet.
  private val successResultSelectSecurities: Result =
    Redirect(routes.SelectSecuritiesController.showFirst())

  // Success: Declaration has been found and ReasonForSecurity is InwardProcessingRelief.
  private val successResultBOD3: Result =
    Redirect(routes.CheckTotalImportDischargedController.show)

  // Success: Declaration has been found and ReasonForSecurity is EndUseRelief.
  private val successResultBOD4: Result =
    Redirect(routes.CheckTotalImportDischargedController.show)

  // Error: Claim has already been submitted as part of a whole or partial claim
  private val errorResultClaimExistsAlready: Result =
    Redirect(controllers.routes.IneligibleController.ineligible) // TODO: fix in CDSR-1773

  import SecuritiesClaim.Checks._

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and TPI04 check has been made.
  override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(hasMRNAndImportDeclarationAndRfS)

  val show: Action[AnyContent] = actionReadClaim { claim =>
    if !claim.needsDeclarantAndConsigneeEoriSubmission then nextPage(claim)
    else if claim.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber).isEmpty then
      Redirect(routes.EnterImporterEoriNumberController.show)
    else Ok(enterDeclarantEoriNumberPage(eoriNumberForm(formKey), postAction))
  }

  val submit: Action[AnyContent] = actionReadWriteClaim { claim =>
    if !claim.needsDeclarantAndConsigneeEoriSubmission then (claim, nextPage(claim))
    else if claim.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber).isEmpty then
      (claim, Redirect(routes.EnterImporterEoriNumberController.show))
    else
      eoriNumberForm(formKey)
        .bindFromRequest()
        .fold(
          formWithErrors => claim -> BadRequest(enterDeclarantEoriNumberPage(formWithErrors, postAction)) asFuture,
          eori =>
            claim
              .submitDeclarantEoriNumber(eori)
              .fold(
                e => {
                  logger.error(
                    s"$eori] does not match EORI associated with MRN [${claim.getDeclarantEoriFromACC14}]: $e"
                  )
                  claim -> BadRequest(
                    enterDeclarantEoriNumberPage(
                      eoriNumberForm(formKey)
                        .withError(
                          FormError(
                            formKey,
                            "eori-should-match-declarant"
                          )
                        )
                        .withDefault(Some(eori)),
                      postAction
                    )
                  ) asFuture
                },
                updatedClaim =>
                  (for
                    mrn                              <- getMovementReferenceNumber(claim)
                    rfs                              <- getReasonForSecurity(claim)
                    similarClaimExistAlreadyInCDFPay <- checkIfClaimIsDuplicated(mrn, rfs)
                    updatedClaimWithRedirect         <- submitClaimDuplicateCheckStatus(
                                                          updatedClaim,
                                                          similarClaimExistAlreadyInCDFPay
                                                        )
                  yield updatedClaimWithRedirect)
                    .bimap(result => (claim, result), identity)
                    .merge
              )
        )
  }

  private def getMovementReferenceNumber(claim: SecuritiesClaim): EitherT[Future, Result, MRN] =
    EitherT.fromOption[Future](
      claim.getLeadMovementReferenceNumber,
      Redirect(routes.EnterMovementReferenceNumberController.show)
    )

  private def getReasonForSecurity(claim: SecuritiesClaim): EitherT[Future, Result, ReasonForSecurity] =
    EitherT.fromOption[Future](
      claim.getReasonForSecurity,
      Redirect(routes.ChooseReasonForSecurityController.show)
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
            } else {
              nextPage(claimWithUpdatedStatus)
            }
          )
        )
        .merge
    )

  private def nextPage(claim: SecuritiesClaim) =
    if claim.reasonForSecurityIsIPR then successResultBOD3
    else if claim.reasonForSecurityIsENU then successResultBOD4
    else successResultSelectSecurities
}
