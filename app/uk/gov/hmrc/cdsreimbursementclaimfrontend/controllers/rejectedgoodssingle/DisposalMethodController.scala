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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class DisposalMethodController @Inject() (
  val jcc: JourneyControllerComponents,
  enterOrChangeMethodOfDisposal: pages.enter_or_change_method_of_disposal
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController
    with Logging {

  private def postAction: Call = routes.DisposalMethodController.submit()

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val mrnContactDetailsForm: Form[MethodOfDisposal] = journey.getMethodOfDisposal
      .map(Forms.methodOfDisposalForm.fill)
      .getOrElse(Forms.methodOfDisposalForm)

    Future.successful(
      Ok(enterOrChangeMethodOfDisposal(mrnContactDetailsForm, postAction))
    )
  }

  def submit(): Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      Forms.methodOfDisposalForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(enterOrChangeMethodOfDisposal(formWithErrors, postAction))
              )
            ),
          methodOfDisposal => {
            val updatedJourney = journey.submitMethodOfDisposal(methodOfDisposal)
            Future.successful(
              (updatedJourney, Redirect(Call("GET", "enter-rejected-goods-details")))
            ) //TODO: replace in CDSR-1137
          }
        )

    }
}
