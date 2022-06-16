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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{securities => pages}

import scala.collection.immutable.ListMap
import scala.concurrent.ExecutionContext

@Singleton
class ChooseReasonForSecurityController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  chooseReasonForSecurityPage: pages.choose_reason_for_security
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  val postAction: Call = routes.ChooseReasonForSecurityController.submit()

  val reasonsForSecurity: Seq[ReasonForSecurity] = ReasonForSecurity.values.toSeq.sorted

  val form: Form[ReasonForSecurity] = Forms.reasonForSecurityForm

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val reasonForSecurityForm: Form[ReasonForSecurity] =
      Forms.reasonForSecurityForm.withDefault(journey.answers.reasonForSecurity)
    Ok(
      chooseReasonForSecurityPage(reasonForSecurityForm, reasonsForSecurity, postAction)
    ).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    form.bindFromRequest
      .fold(
        formWithErrors =>
          (
            journey,
            BadRequest(chooseReasonForSecurityPage(formWithErrors, reasonsForSecurity, postAction))
          ).asFuture,
        reasonForSecurity => (journey, Ok(s"We got a reason for security: $reasonForSecurity")).asFuture
      )
  }

}
