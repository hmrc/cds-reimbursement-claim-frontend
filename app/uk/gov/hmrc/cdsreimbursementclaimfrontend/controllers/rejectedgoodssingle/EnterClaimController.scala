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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  enterClaim: pages.enter_claim
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  val key = "enter-claim.rejected-goods.single"

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.getNextTaxCodeToClaim match {
      case Some(ndrcDetails) =>
        val amountPaid = BigDecimal(ndrcDetails.amount)
        val form = Forms.claimAmountForm(ndrcDetails.taxType, amountPaid)
        Future.successful(Ok(enterClaim(form, TaxCode(ndrcDetails.taxType), amountPaid, routes.EnterClaimController.submit())))
    }
  }

  def submit(): Action[AnyContent] = Action { implicit request =>
    val form = Forms.claimAmountForm(key, 100)
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          BadRequest(enterClaim(formWithErrors, TaxCode.A00, 100, routes.EnterClaimController.submit())),
        data => Ok(s"We got this data - $data")
      )
  }
}
