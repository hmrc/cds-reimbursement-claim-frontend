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

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MRNMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkDeclarationDetailsPage: pages.check_declaration_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  implicit val subKey: Option[String] = MRNMultipleRoutes.subKey

  private val postAction: Call                       = routes.CheckDeclarationDetailsController.submit()
  val checkDeclarationDetailsKey: String             = "check-declaration-details"
  val checkDeclarationDetailsAnswerForm: Form[YesNo] = YesOrNoQuestionForm(checkDeclarationDetailsKey)

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.answers.displayDeclarations
      .flatMap(declarations => declarations.headOption)
      .fold(Redirect("enter-movement-reference-number"))(declaration =>
        Ok(checkDeclarationDetailsPage(declaration, checkDeclarationDetailsAnswerForm, isDuplicate = false, postAction))
      )
      .asFuture
  }

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
              case Yes =>
                val numOfMRNs = journey.countOfMovementReferenceNumbers
                if (numOfMRNs > 1)
                  routes.CheckMovementReferenceNumbersController.show()
                else
                  routes.EnterMovementReferenceNumberController.show(numOfMRNs + 1)
              case No  =>
                routes.EnterMovementReferenceNumberController.showFirst()
            })
          )
      )
  }
}
