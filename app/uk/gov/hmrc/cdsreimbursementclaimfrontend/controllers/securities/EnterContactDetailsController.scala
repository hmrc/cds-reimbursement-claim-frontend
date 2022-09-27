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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import com.github.arturopala.validator.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{securities => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterContactDetailsController @Inject() (
  enterOrChangeContactDetailsPage: pages.enter_or_change_contact_details,
  val jcc: JourneyControllerComponents
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  private def postAction: Call = routes.EnterContactDetailsController.submit()

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  val show: Action[AnyContent] =
    actionReadJourneyAndUser { implicit request => journey => userType =>
      Future.successful(
        Ok(
          enterOrChangeContactDetailsPage(
            Forms.securitiesContactDetailsForm.withDefault(journey.computeContactDetails(userType)),
            postAction
          )
        )
      )
    }

  val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      Forms.securitiesContactDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors => (journey, BadRequest(enterOrChangeContactDetailsPage(formWithErrors, postAction))).asFuture,
          contactDetails => {
            val updatedJourney = journey.submitContactDetails(Some(contactDetails))
            (updatedJourney, Redirect(routes.CheckClaimantDetailsController.show())).asFuture
          }
        )
    }
}
