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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{securities => pages}
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseReasonForSecurityController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  chooseReasonForSecurityPage: pages.choose_reason_for_security
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends SecuritiesJourneyBaseController {

  val postAction: Call = routes.ChooseReasonForSecurityController.submit()

  val reasonsForSecurity: Seq[ReasonForSecurity] = ReasonForSecurity.values.toSeq.sorted

  val form: Form[ReasonForSecurity] = Forms.reasonForSecurityForm

  def show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val reasonForSecurityForm: Form[ReasonForSecurity] =
      Forms.reasonForSecurityForm.withDefault(journey.answers.reasonForSecurity)
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
        reasonForSecurity => updateAndRedirect(reasonForSecurity, journey)
      )
  }

  private def updateAndRedirect(reasonForSecurity: ReasonForSecurity, journey: SecuritiesJourney)(implicit
    request: Request[_],
    hc: HeaderCarrier
  ): Future[(SecuritiesJourney, Result)] = {

    def withJourney(result: Result, securitiesJourney: SecuritiesJourney = journey): (SecuritiesJourney, Result) =
      (securitiesJourney, result)

    val displayDeclarationFromAcc14: EitherT[Future, (SecuritiesJourney, Result), Option[DisplayDeclaration]] = for {
      mrn         <- EitherT
                       .fromOption[Future](journey.getLeadMovementReferenceNumber, Error("Could not get MRN"))
                       .leftMap(error => withJourney(logAndDisplayError("Could not get MRN", error)))
      declaration <-
        claimService
          .getDisplayDeclaration(mrn, reasonForSecurity)
          .leftMap(error => withJourney(logAndDisplayError("Could not get DisplayDeclaration", error)))
    } yield declaration

    def checkWhetherEORIsMatch: EitherT[Future, (SecuritiesJourney, Result), Boolean] =
      for (declaration <- displayDeclarationFromAcc14)
        yield declaration
          .map((dd: DisplayDeclaration) => dd.getConsigneeEori.toList ::: List[Eori](dd.getDeclarantEori))
          .exists(_.contains(journey.answers.userEoriNumber))

    def updateJourney(dd: DisplayDeclaration): EitherT[Future, (SecuritiesJourney, Result), SecuritiesJourney] =
      EitherT
        .fromEither[Future](journey.submitReasonForSecurityAndDeclaration(reasonForSecurity, dd))
        .leftMap(_ => withJourney(Redirect(controllers.routes.IneligibleController.ineligible())))

    val updateAndRedirect: EitherT[Future, (SecuritiesJourney, Result), (SecuritiesJourney, Result)] = for {
      maybeDD    <- displayDeclarationFromAcc14
      eorisMatch <- checkWhetherEORIsMatch
      status     <- (maybeDD, eorisMatch) match {
                      case (Some(dd), true)  =>
                        updateJourney(dd).map(updatedJourney =>
                          //TODO: https://jira.tools.tax.service.gov.uk/browse/CDSR-1770
                          withJourney(Redirect(Call("GET", "Call TPI04")), updatedJourney)
                        )
                      case (Some(dd), false) =>
                        updateJourney(dd).map(updatedJourney =>
                          withJourney(Redirect(routes.EnterImporterEoriNumberController.show()), updatedJourney)
                        )
                      case (_, _)            =>
                        EitherT.rightT[Future, (SecuritiesJourney, Result)](
                          withJourney(Redirect(controllers.routes.IneligibleController.ineligible()))
                        )
                    }
    } yield status

    updateAndRedirect.merge
  }
}
