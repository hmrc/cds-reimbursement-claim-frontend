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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{common => pages}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.language.postfixOps

@Singleton
class EnterDeclarantEoriNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  DeclarationConnector: DeclarationConnector,
  enterDeclarantEoriNumberPage: pages.enter_declarant_eori_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends SecuritiesJourneyBaseController {

  val formKey: String  = "enter-declarant-eori-number"
  val postAction: Call = routes.EnterDeclarantEoriNumberController.submit()

  //Success: Declaration has been found and claim for this MRN and RfS does not exist yet.
  private val successResultSelectSecurities: Result =
    Redirect(routes.SelectSecuritiesController.showFirst())

  //Success: Declaration has been found and ReasonForSecurity is InwardProcessingRelief.
  private val successResultBOD3: Result =
    Redirect(routes.BillOfDischarge3Controller.show())

  //Success: Declaration has been found and ReasonForSecurity is EndUseRelief.
  private val successResultBOD4: Result =
    Redirect(routes.BillOfDischarge4Controller.show())

  //Error: Claim has already been submitted as part of a whole or partial claim
  private val errorResultClaimExistsAlready: Result =
    Redirect(controllers.routes.IneligibleController.ineligible()) // TODO: fix in CDSR-1773

  import SecuritiesJourney.Checks._

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and TPI04 check has been made.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(hasMRNAndDisplayDeclarationAndRfS)

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    (
      if (!journey.needsDeclarantAndConsigneeEoriSubmission) nextPage(journey)
      else if (journey.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber).isEmpty)
        Redirect(routes.EnterImporterEoriNumberController.show())
      else Ok(enterDeclarantEoriNumberPage(eoriNumberForm(formKey), postAction))
    ).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    if (!journey.needsDeclarantAndConsigneeEoriSubmission) (journey, nextPage(journey)).asFuture
    else if (journey.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber).isEmpty)
      (journey, Redirect(routes.EnterImporterEoriNumberController.show())).asFuture
    else
      eoriNumberForm(formKey)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            journey -> BadRequest(enterDeclarantEoriNumberPage(formWithErrors.fill(Eori("")), postAction)) asFuture,
          eori =>
            journey
              .submitDeclarantEoriNumber(eori)
              .fold(
                e => {
                  logger
                    .error(s"$eori] does not match EORI associated with MRN [${journey.getDeclarantEoriFromACC14}]: $e")
                  journey -> Redirect(controllers.routes.IneligibleController.ineligible()) asFuture
                },
                updatedJourney =>
                  (for {
                    mrn                              <- getMovementReferenceNumber(journey)
                    rfs                              <- getReasonForSecurity(journey)
                    similarClaimExistAlreadyInCDFPay <- checkIfClaimIsDuplicated(mrn, rfs)
                    updatedJourneyWithRedirect       <- submitClaimDuplicateCheckStatus(
                                                          updatedJourney,
                                                          similarClaimExistAlreadyInCDFPay
                                                        )
                  } yield updatedJourneyWithRedirect)
                    .bimap(result => (journey, result), identity)
                    .merge
              )
        )
  }

  private def getMovementReferenceNumber(journey: SecuritiesJourney): EitherT[Future, Result, MRN] =
    EitherT.fromOption[Future](
      journey.getLeadMovementReferenceNumber,
      Redirect(routes.EnterMovementReferenceNumberController.show())
    )

  private def getReasonForSecurity(journey: SecuritiesJourney): EitherT[Future, Result, ReasonForSecurity] =
    EitherT.fromOption[Future](
      journey.getReasonForSecurity,
      Redirect(routes.ChooseReasonForSecurityController.show())
    )

  private def checkIfClaimIsDuplicated(mrn: MRN, reasonForSecurity: ReasonForSecurity)(implicit
    hc: HeaderCarrier,
    r: Request[_]
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
            if (similarClaimExistAlreadyInCDFPay) {
              logger.info("Claim ineligible because already exists.")
              errorResultClaimExistsAlready
            } else {
              nextPage(journeyWithUpdatedStatus)
            }
          )
        )
        .merge
    )

  private def nextPage(journey: SecuritiesJourney) =
    if (journey.reasonForSecurityIsIPR) successResultBOD3
    else if (journey.reasonForSecurityIsEndUseRelief) successResultBOD4
    else successResultSelectSecurities
}
