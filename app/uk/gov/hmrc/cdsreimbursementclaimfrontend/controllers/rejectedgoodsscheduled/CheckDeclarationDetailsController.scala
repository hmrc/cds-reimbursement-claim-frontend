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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkDeclarationDetailsPage: pages.check_declaration_details
)(implicit viewConfig: ViewConfig, errorHandler: ErrorHandler, ec: ExecutionContext)
    extends RejectedGoodsScheduledJourneyBaseController {

  implicit val subKey: Option[String] = Some("scheduled")

  val checkDeclarationDetailsKey: String = s"check-declaration-details${subKey.fold("")(a => s".$a")}"

  val checkDeclarationDetailsAnswerForm: Form[YesNo] =
    YesOrNoQuestionForm(checkDeclarationDetailsKey)

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val postAction: Call = routes.CheckDeclarationDetailsController.submit()
    Future.successful(
      journey.answers.displayDeclaration.fold(Redirect(baseRoutes.IneligibleController.ineligible()))(declaration =>
        Ok(checkDeclarationDetailsPage(declaration, checkDeclarationDetailsAnswerForm, isDuplicate = false, postAction))
      )
    )
  }

  val submit: Action[AnyContent] = simpleActionReadWriteJourney { implicit request => journey =>
    val postAction: Call = routes.CheckDeclarationDetailsController.submit()
    checkDeclarationDetailsAnswerForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            journey.answers.displayDeclaration
              .map(declaration =>
                BadRequest(
                  checkDeclarationDetailsPage(
                    declaration,
                    formWithErrors,
                    isDuplicate = false,
                    postAction
                  )
                )
              )
              .getOrElse(errorHandler.errorResult())
          ),
        answer =>
          (
            journey,
            Redirect(answer match {
              case Yes =>
                routes.UploadMrnListController.show()
              case No  =>
                routes.EnterMovementReferenceNumberController.submit()
            })
          )
      )
  }

}
