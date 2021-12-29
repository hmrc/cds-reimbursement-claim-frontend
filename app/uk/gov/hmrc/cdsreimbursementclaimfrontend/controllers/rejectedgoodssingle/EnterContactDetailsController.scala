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

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterContactDetailsMrnController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterContactDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  enterOrChangeContactDetailsPage: pages.enter_or_change_contact_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController
    with Logging {

  implicit val dataExtractor: DraftClaim => Option[MrnContactDetails] = _.mrnContactDetailsAnswer

  private val postAction = routes.EnterContactDetailsController.submit()

  def show(): Action[AnyContent] = simpleActionReadJourneyAndUser { implicit request => _ => _ =>
    Future.successful(
      Ok(enterOrChangeContactDetailsPage(EnterContactDetailsMrnController.mrnContactDetailsForm, postAction))
    )
  }

  def submit(): Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      EnterContactDetailsMrnController.mrnContactDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful((journey, BadRequest(enterOrChangeContactDetailsPage(formWithErrors, postAction)))),
          contactDetails => {
            val updatedJourney = journey.submitContactDetails(Some(contactDetails))
            Future.successful((updatedJourney, Redirect(routes.CheckClaimantDetailsController.show())))
          }
        )

    }
}
