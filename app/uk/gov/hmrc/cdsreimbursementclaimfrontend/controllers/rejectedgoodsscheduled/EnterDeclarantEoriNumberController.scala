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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.eoriNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import scala.concurrent.ExecutionContext

@Singleton
class EnterDeclarantEoriNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  enterDeclarantEoriNumber: pages.enter_declarant_eori_number
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  val eoriNumberFormKey: String       = "enter-declarant-eori-number"
  private val declarantEoriNumberForm = eoriNumberForm(eoriNumberFormKey)
  private val postAction              = routes.EnterDeclarantEoriNumberController.submit()

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      enterDeclarantEoriNumber(
        declarantEoriNumberForm.withDefault(journey.answers.declarantEoriNumber),
        postAction
      )
    ).asFuture
  }

  def submit(): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    declarantEoriNumberForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(
              enterDeclarantEoriNumber(
                formWithErrors.fill(Eori("")),
                postAction
              )
            )
          ),
        eori =>
          journey
            .submitDeclarantEoriNumber(eori)
            .fold(
              errors => {
                logger.error(s"Unable to record $eori - $errors")
                (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
              },
              updatedJourney => (updatedJourney, Redirect("check-declaration-details")) //TODO: Fix with proper route
            )
      )
      .asFuture
  }
}
