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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.check_claim_details

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetailsPage: check_claim_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {
  val postAction: Call = routes.CheckClaimDetailsController.submit();

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.getLeadDisplayDeclaration
      .fold(Redirect(routes.EnterMovementReferenceNumberController.show())) { displayDeclaration =>
        Ok(
          checkClaimDetailsPage(displayDeclaration, journey.getSecuritiesReclaims, postAction)
        )
      }
      .asFuture

  }

  val submit: Action[AnyContent] = actionReadJourney { _ => journey =>
    if (journey.answers.reasonForSecurity.exists(ReasonForSecurity.requiresDocumentType.contains)) {
      Redirect(routes.ChooseFileTypeController.show()).asFuture
    } else {
      Redirect(routes.UploadFilesController.show()).asFuture
    }
  }

}
