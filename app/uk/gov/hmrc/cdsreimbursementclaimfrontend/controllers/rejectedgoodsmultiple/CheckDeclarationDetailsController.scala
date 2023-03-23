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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import play.api.data.Form
import play.api.mvc.Call
import play.api.mvc.Request
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.CheckDeclarationDetailsMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MRNMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.check_declaration_details

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkDeclarationDetailsPage: check_declaration_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends RejectedGoodsMultipleJourneyBaseController
    with CheckDeclarationDetailsMixin {

  final override def getDisplayDeclaration(journey: Journey): Option[DisplayDeclaration] =
    journey.getLeadDisplayDeclaration

  final override def continueRoute(journey: Journey): Call = {
    val numOfMRNs = journey.countOfMovementReferenceNumbers
    if (numOfMRNs > 1)
      routes.CheckMovementReferenceNumbersController.show()
    else
      routes.EnterMovementReferenceNumberController.show(numOfMRNs + 1)
  }

  final override val enterMovementReferenceNumberRoute: Call =
    routes.EnterMovementReferenceNumberController.showFirst()

  private val postAction: Call =
    routes.CheckDeclarationDetailsController.submit()

  override def viewTemplate: (DisplayDeclaration, Form[YesNo]) => Request[_] => HtmlFormat.Appendable = {
    case (decl, form) =>
      implicit request =>
        checkDeclarationDetailsPage(decl, form, false, postAction, MRNMultipleRoutes.subKey)
  }

}
