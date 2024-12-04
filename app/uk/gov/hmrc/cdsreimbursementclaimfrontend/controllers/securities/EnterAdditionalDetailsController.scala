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
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_additional_details

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterAdditionalDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  enterAdditionalDetailsPage: enter_additional_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val postAction: Call    = routes.EnterAdditionalDetailsController.submit
  final val continueRoute: Call = routes.CheckYourAnswersController.show

  final val show: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      Future.successful {
        val form: Form[String] =
          Forms.enterAdditionalDetailsForm.withDefault(journey.answers.additionalDetails)
        Ok(
          enterAdditionalDetailsPage(
            form,
            postAction
          )
        )
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      Forms.enterAdditionalDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(
                  enterAdditionalDetailsPage(
                    formWithErrors,
                    postAction
                  )
                )
              )
            ),
          additionalDetails =>
            Future.successful(
              (
                journey.submitAdditionalDetails(additionalDetails),
                Redirect(continueRoute)
              )
            )
        )
    }

}
