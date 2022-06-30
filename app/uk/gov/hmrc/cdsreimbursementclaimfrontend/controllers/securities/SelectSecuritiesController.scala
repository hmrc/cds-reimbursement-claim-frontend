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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectSecuritiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{securities => pages}

import scala.concurrent.ExecutionContext

@Singleton
class SelectSecuritiesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectSecuritiesPage: pages.select_securities
)(implicit viewConfig: ViewConfig, errorHandler: ErrorHandler, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController
    with Logging
    with SecuritiesJourneyRouter {

  private val form: Form[YesNo] = selectSecuritiesForm

  def show(id: String): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val postAction: Call = routes.SelectSecuritiesController.submit(id)
    journey.answers.displayDeclaration
      .fold(Redirect(baseRoutes.IneligibleController.ineligible()))(declaration =>
        Ok(selectSecuritiesPage(form, declaration, id, postAction))
      )
      .asFuture
  }

  def submit(id: String): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val postAction: Call = routes.SelectSecuritiesController.submit(id)
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            journey.answers.displayDeclaration
              .map(declaration => BadRequest(selectSecuritiesPage(formWithErrors, declaration, id, postAction)))
              .getOrElse(errorHandler.errorResult())
          ).asFuture,
        securities => ??? // selectSecurityDepositId
      )
  }

}

object SelectSecuritiesController {
  val selectSecuritiesKey: String = "select-securities.securities"
}
