/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterImporterEoriNumberController.eoriNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterImporterEoriNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  enterImporterEoriNumber: pages.enter_importer_eori_number
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      Ok(
        enterImporterEoriNumber(
          eoriNumberForm.withDefault(journey.answers.consigneeEoriNumber),
          routes.EnterImporterEoriNumberController.submit()
        )
      )
    }
  }

  def submit(): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    eoriNumberForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Future.successful(
            (
              journey,
              BadRequest(
                enterImporterEoriNumber(
                  formWithErrors.fill(Eori("")),
                  routes.EnterImporterEoriNumberController.submit()
                )
              )
            )
          ),
        eori =>
          Future.successful(
            journey
              .submitConsigneeEoriNumber(eori)
              .fold(
                errors => {
                  logger.error(s"Unable to record $eori - $errors")
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                },
                updatedJourney => (updatedJourney, Redirect("rejected-goods/single/enter-declarant-eori"))
              )
          )
      )
  }
}
