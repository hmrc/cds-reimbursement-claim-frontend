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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.problem_with_declaration_can_continue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.problem_with_declaration_dead_end
import play.api.data.Form
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms

trait ProblemWithDeclarationMixin extends JourneyBaseController {
  def removeUnsupportedTaxCodesFromJourney(journey: Journey): Journey
  val problemWithDeclarationCanContinuePage: problem_with_declaration_can_continue
  val problemWithDeclarationDeadEndPage: problem_with_declaration_dead_end
  val postAction: Call
  val enterAnotherMrnAction: Call
  val checkDeclarationDetailsAction: Call

  final val show: Action[AnyContent] =
    actionReadJourney { implicit request => implicit journey =>
      val form: Form[YesNo] = Forms.problemWithDeclarationForm
      (journey.getLeadDisplayDeclaration match {
        case Some(declaration) if declaration.containsOnlyUnsupportedTaxCodes =>
          Ok(problemWithDeclarationDeadEndPage(declaration.getMRN, enterAnotherMrnAction))
        case Some(declaration) if declaration.containsSomeUnsupportedTaxCode  =>
          Ok(problemWithDeclarationCanContinuePage(form, declaration.getMRN, postAction))
        case Some(_)                                                          =>
          Redirect(checkDeclarationDetailsAction)
        case None                                                             => ???
      }).asFuture
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => implicit journey =>
      Forms.problemWithDeclarationForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              journey.getLeadDisplayDeclaration
                .map { declaration =>
                  BadRequest(
                    problemWithDeclarationCanContinuePage(formWithErrors, declaration.getMRN, postAction)
                  )
                }
                .getOrElse(InternalServerError)
            ).asFuture,
          answer =>
            answer match {
              case YesNo.No  =>
                (journey, Redirect(enterAnotherMrnAction)).asFuture
              case YesNo.Yes =>
                (removeUnsupportedTaxCodesFromJourney(journey), Redirect(checkDeclarationDetailsAction)).asFuture
            }
        )
    }
}
