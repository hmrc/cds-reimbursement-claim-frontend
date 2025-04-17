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

import com.github.arturopala.validator.Validator.Validate
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.ProblemWithDeclarationMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney.Checks.hasMRNAndDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.problem_with_declaration_can_continue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.problem_with_declaration_dead_end

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class ProblemWithDeclarationController @Inject() (
  val jcc: JourneyControllerComponents,
  override val problemWithDeclarationCanContinuePage: problem_with_declaration_can_continue,
  override val problemWithDeclarationDeadEndPage: problem_with_declaration_dead_end
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsMultipleJourneyBaseController
    with ProblemWithDeclarationMixin {

  override def removeUnsupportedTaxCodesFromJourney(
    journey: OverpaymentsMultipleJourney
  ): OverpaymentsMultipleJourney =
    journey.removeUnsupportedTaxCodes()

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsMultipleJourney]] =
    Some(hasMRNAndDisplayDeclaration)

  final override val postAction: Call =
    routes.ProblemWithDeclarationController.submit

  final override val enterAnotherMrnAction: Call =
    routes.EnterMovementReferenceNumberController.showFirst

  final override val checkDeclarationDetailsAction: Call =
    routes.CheckDeclarationDetailsController.show

  final def showNth(pageIndex: Int): Action[AnyContent] =
    actionReadJourney { implicit request => implicit journey =>
      val form: Form[YesNo] = Forms.problemWithDeclarationForm
      (journey.getNthDisplayDeclaration(pageIndex - 1) match {
        case Some(declaration) if declaration.containsOnlyUnsupportedTaxCodes =>
          Ok(
            problemWithDeclarationDeadEndPage(
              declaration.getMRN,
              routes.EnterMovementReferenceNumberController.show(pageIndex),
              getFormMessageKeyAndUrl(journey)
            )
          )
        case Some(declaration) if declaration.containsSomeUnsupportedTaxCode  =>
          Ok(
            problemWithDeclarationCanContinuePage(
              form,
              declaration.getMRN,
              routes.ProblemWithDeclarationController.submitNth(pageIndex),
              getFormMessageKeyAndUrl(journey)
            )
          )
        case Some(_)                                                          =>
          Redirect(routes.CheckMovementReferenceNumbersController.show)
        case None                                                             =>
          throw new IllegalStateException(s"Expected the journey to have $pageIndex DisplayDeclaration already")
      }).asFuture
    }

  final def submitNth(pageIndex: Int): Action[AnyContent] =
    actionReadWriteJourney { implicit request => implicit journey =>
      Forms.problemWithDeclarationForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              journey
                .getNthDisplayDeclaration(pageIndex - 1)
                .map { declaration =>
                  BadRequest(
                    problemWithDeclarationCanContinuePage(
                      formWithErrors,
                      declaration.getMRN,
                      routes.ProblemWithDeclarationController.submitNth(pageIndex),
                      getFormMessageKeyAndUrl(journey)
                    )
                  )
                }
                .getOrElse(InternalServerError)
            ).asFuture,
          answer =>
            answer match {
              case YesNo.No  =>
                (journey, Redirect(routes.EnterMovementReferenceNumberController.show(pageIndex))).asFuture
              case YesNo.Yes =>
                (
                  removeUnsupportedTaxCodesFromJourney(journey),
                  Redirect(routes.CheckMovementReferenceNumbersController.show)
                ).asFuture
            }
        )
    }
}
