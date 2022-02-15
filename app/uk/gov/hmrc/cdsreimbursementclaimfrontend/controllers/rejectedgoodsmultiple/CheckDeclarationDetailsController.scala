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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.checkDeclarationDetailsAnswerForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarantDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayResponseDetail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}
import scala.concurrent.ExecutionContext

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkDeclarationDetailsPage: pages.check_declaration_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController
    with Logging {

  implicit val subKey: Option[String] = Some("multiple")

  val postAction: Call = routes.CheckDeclarationDetailsController.submit()

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.answers.displayDeclarations
      .flatMap(declarations => declarations.headOption)
      .fold(Redirect("enter-movement-reference-number"))(declaration =>
        Ok(checkDeclarationDetailsPage(declaration, checkDeclarationDetailsAnswerForm, isDuplicate = false, postAction))
      )
      .asFuture
  }

  /*

   */
  def submit(): Action[AnyContent] = simpleActionReadWriteJourney { implicit request => journey =>
    checkDeclarationDetailsAnswerForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            journey.answers.displayDeclarations
              .flatMap(declarations => declarations.headOption)
              .fold(Redirect("enter-movement-reference-number"))(declaration =>
                BadRequest(
                  checkDeclarationDetailsPage(
                    declaration,
                    formWithErrors,
                    isDuplicate = false,
                    postAction
                  )
                )
              )
          ),
        answer =>
          (
            journey,
            Redirect(answer match {
              case Yes => "/enter-movement-reference-number/2"
              case No  => "enter-movement-reference-number"
            })
          )
      )
  }
}