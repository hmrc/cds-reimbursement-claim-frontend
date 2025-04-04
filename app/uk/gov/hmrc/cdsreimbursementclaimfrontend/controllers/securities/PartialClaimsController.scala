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

import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.partialClaimsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.routes as commonRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.partial_claims

import scala.concurrent.ExecutionContext

@Singleton
class PartialClaimsController @Inject() (
  val jcc: JourneyControllerComponents,
  partialClaimsPage: partial_claims
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesJourneyBaseController
    with SecuritiesJourneyRouter
    with Logging {

  private val form: Form[YesNo] = partialClaimsForm

  import SecuritiesJourney.Checks.*

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  def show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val postAction: Call = routes.PartialClaimsController.submit
    Ok(
      partialClaimsPage(
        form,
        postAction
      )
    ).asFuture
  }

  def submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val postAction: Call = routes.PartialClaimsController.submit
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(partialClaimsPage(formWithErrors, postAction))
          ).asFuture,
        yesNo =>
          (yesNo match {
            case YesNo.Yes =>
              (journey, Redirect(routes.SelectDutiesController.showFirst))
            case YesNo.No  =>
              (
                journey,
                Redirect(routes.ClaimDeletedController.show)
              )
          }).asFuture
      )
  }
}
