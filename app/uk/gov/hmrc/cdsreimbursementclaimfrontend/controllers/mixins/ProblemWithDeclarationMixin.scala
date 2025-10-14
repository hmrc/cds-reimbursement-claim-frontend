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

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.FormMessageKeyAndUrl
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.problem_with_declaration_can_continue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.problem_with_declaration_dead_end

trait ProblemWithDeclarationMixin extends ClaimBaseController {
  def removeUnsupportedTaxCodesFromClaim(claim: Claim): Claim
  val problemWithDeclarationCanContinuePage: problem_with_declaration_can_continue
  val problemWithDeclarationDeadEndPage: problem_with_declaration_dead_end
  val postAction: Call
  val enterAnotherMrnAction: Call
  val checkDeclarationDetailsAction: Call

  def getFormMessageKeyAndUrl(claim: ClaimBase): FormMessageKeyAndUrl =
    claim match {
      case j @ (_: OverpaymentsSingleClaim | _: OverpaymentsMultipleClaim | _: OverpaymentsScheduledClaim) =>
        FormMessageKeyAndUrl("problem-with-declaration.c285-form", viewConfig.legacyC285FormUrl)
      case _                                                                                               =>
        FormMessageKeyAndUrl("problem-with-declaration.ce1179-form", viewConfig.ce1179FormUrl)
    }

  final val show: Action[AnyContent] =
    actionReadClaim { implicit claim =>
      val form: Form[YesNo] = Forms.problemWithDeclarationForm
      claim.getLeadImportDeclaration match {
        case Some(declaration) if declaration.containsOnlyUnsupportedTaxCodes =>
          Ok(
            problemWithDeclarationDeadEndPage(
              declaration.getMRN,
              enterAnotherMrnAction,
              getFormMessageKeyAndUrl(claim)
            )
          )
        case Some(declaration) if declaration.containsSomeUnsupportedTaxCode  =>
          Ok(
            problemWithDeclarationCanContinuePage(
              form,
              declaration.getMRN,
              postAction,
              getFormMessageKeyAndUrl(claim)
            )
          )
        case Some(_)                                                          =>
          Redirect(checkDeclarationDetailsAction)
        case None                                                             =>
          throw new IllegalStateException("Expected the claim to have ImportDeclaration already")
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim { implicit claim =>
      Forms.problemWithDeclarationForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              claim.getLeadImportDeclaration
                .map { declaration =>
                  BadRequest(
                    problemWithDeclarationCanContinuePage(
                      formWithErrors,
                      declaration.getMRN,
                      postAction,
                      getFormMessageKeyAndUrl(claim)
                    )
                  )
                }
                .getOrElse(InternalServerError)
            ),
          answer =>
            answer match {
              case YesNo.No  =>
                (claim, Redirect(enterAnotherMrnAction))
              case YesNo.Yes =>
                (removeUnsupportedTaxCodesFromClaim(claim), Redirect(checkDeclarationDetailsAction))
            }
        )
    }
}
