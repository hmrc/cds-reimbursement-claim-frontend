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
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.CDSReimbursementClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.eoriNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.language.postfixOps

@Singleton
class EnterDeclarantEoriNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  cdsReimbursementClaimConnector: CDSReimbursementClaimConnector,
  enterDeclarantEoriNumberPage: pages.enter_declarant_eori_number
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends SecuritiesJourneyBaseController {

  val formKey: String  = "enter-declarant-eori-number"
  val postAction: Call = routes.EnterDeclarantEoriNumberController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Ok(enterDeclarantEoriNumberPage(eoriNumberForm(formKey), postAction)).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
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
              updatedJourney => nextPage(updatedJourney)
            )
      )
  }

  def nextPage(
    journey: SecuritiesJourney
  )(implicit request: Request[_], hc: HeaderCarrier): Future[(SecuritiesJourney, Result)] = {
    def withJourney(result: Result): (SecuritiesJourney, Result) = (journey, result)

    def isDuplicateClaim: EitherT[Future, (SecuritiesJourney, Result), Boolean] = for {
      mrn               <- EitherT
                             .fromOption[Future](journey.getLeadMovementReferenceNumber, Error("Could not get MRN"))
                             .leftMap(error => withJourney(logAndDisplayError("Failed to get lead MRN from journey", error)))
      reasonForSecurity <-
        EitherT
          .fromOption[Future](
            journey.getReasonForSecurity,
            Error("Could not get ReasonForSecurity")
          )
          .leftMap(error => withJourney(logAndDisplayError("Failed to get ReasonForSecurity from journey", error)))
      existingClaim     <-
        cdsReimbursementClaimConnector
          .getIsDuplicate(mrn, reasonForSecurity)
          .leftMap(error => withJourney(logAndDisplayError("Could not check if isDuplicate claim", error)))
    } yield existingClaim.claimFound

    val updateAndRedirect = for {
      isDuplicate <- isDuplicateClaim
      result      <- if (isDuplicate) {
                       EitherT.rightT[Future, (SecuritiesJourney, Result)](
                         withJourney(Redirect(controllers.routes.IneligibleController.ineligible()))
                       )
                     } else {
                       EitherT.rightT[Future, (SecuritiesJourney, Result)](
                         withJourney(Redirect(routes.SelectDutiesController.show(""))) //TODO: CDSR-1551
                       )
                     }
    } yield result

    updateAndRedirect.merge
  }
}
