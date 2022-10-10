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
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.eoriNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import com.github.arturopala.validator.Validator.Validate

@Singleton
class EnterImporterEoriNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  enterImporterEoriNumberPage: pages.enter_importer_eori_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  val eoriNumberFormKey: String = "enter-importer-eori-number"
  val postAction: Call          = routes.EnterImporterEoriNumberController.submit()

  import SecuritiesJourney.Checks._

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and TPI04 check has been made.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(hasMRNAndDisplayDeclarationAndRfS)

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    (if (!journey.needsDeclarantAndConsigneeEoriSubmission)
       Redirect(routes.SelectSecuritiesController.showFirst())
     else
       Ok(
         enterImporterEoriNumberPage(
           eoriNumberForm(eoriNumberFormKey).withDefault(journey.answers.consigneeEoriNumber),
           postAction
         )
       )).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    if (!journey.needsDeclarantAndConsigneeEoriSubmission)
      (journey, Redirect(routes.SelectSecuritiesController.showFirst())).asFuture
    else
      eoriNumberForm(eoriNumberFormKey)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(
                  enterImporterEoriNumberPage(
                    formWithErrors.fill(Eori("")),
                    postAction
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
                  updatedJourney =>
                    (
                      updatedJourney,
                      Redirect(routes.EnterDeclarantEoriNumberController.show())
                    )
                )
            )
        )
  }

}
