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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterSpecialCircumstancesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterSpecialCircumstancesController @Inject() (
  val jcc: JourneyControllerComponents,
  enterSpecialCircumstancesPage: pages.enter_special_circumstances
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  val formKey: String          = "enter-special-circumstances.rejected-goods"
  private val postAction: Call = routes.EnterSpecialCircumstancesController.submit()

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      val form = enterSpecialCircumstancesForm.withDefault(journey.answers.basisOfClaimSpecialCircumstances)

      Ok(enterSpecialCircumstancesPage(form, postAction))
    }
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    Future.successful(
      enterSpecialCircumstancesForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                enterSpecialCircumstancesPage(
                  formWithErrors,
                  postAction
                )
              )
            ),
          specialCircumstances =>
            journey
              .submitBasisOfClaimSpecialCircumstancesDetails(specialCircumstances)
              .fold(
                errors => {
                  logger.error(s"unable to match basis of claim - $errors")
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                },
                updatedJourney => (updatedJourney, Redirect(routes.DisposalMethodController.show()))
              )
        )
    )
  }

}
