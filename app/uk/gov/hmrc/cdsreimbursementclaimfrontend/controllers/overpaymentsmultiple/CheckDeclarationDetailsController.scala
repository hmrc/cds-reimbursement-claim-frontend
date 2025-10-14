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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

import play.api.mvc.Call
import play.api.mvc.Request
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.CheckDeclarationDetailsMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.check_declaration_details

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val jcc: ClaimControllerComponents,
  checkDeclarationDetailsPage: check_declaration_details
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext, val errorHandler: ErrorHandler)
    extends OverpaymentsMultipleClaimBaseController
    with CheckDeclarationDetailsMixin {

  final override def getImportDeclaration(claim: Claim): Option[ImportDeclaration] =
    claim.getLeadImportDeclaration

  final override def continueRoute(claim: Claim): Call = {
    val numOfMRNs = claim.countOfMovementReferenceNumbers
    if numOfMRNs > 1 then routes.CheckMovementReferenceNumbersController.show
    else routes.EnterMovementReferenceNumberController.show(numOfMRNs + 1)
  }

  final override val enterMovementReferenceNumberRoute: Call =
    routes.EnterMovementReferenceNumberController.showFirst

  private val postAction: Call =
    routes.CheckDeclarationDetailsController.submit

  override def viewTemplate: (ImportDeclaration, Claim) => Request[?] ?=> HtmlFormat.Appendable = {
    case (decl, claim) =>
      checkDeclarationDetailsPage(
        declaration = decl,
        isDuplicate = false,
        postAction = postAction,
        enterMovementReferenceNumberRoute
      )
  }

}
