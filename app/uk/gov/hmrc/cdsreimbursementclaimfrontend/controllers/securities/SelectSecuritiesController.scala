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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectSecuritiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.select_securities

import scala.concurrent.ExecutionContext

@Singleton
class SelectSecuritiesController @Inject() (
  val jcc: ClaimControllerComponents,
  selectSecuritiesPage: select_securities
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesClaimBaseController
    with SecuritiesClaimRouter
    with Logging {

  private val form: Form[YesNo] = selectSecuritiesForm

  import SecuritiesClaim.Checks._

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final val showFirst: Action[AnyContent] = simpleActionReadWriteClaim { claim =>
    claim.getSecurityDepositIds.headOption.fold(
      (claim, Redirect(routes.ChooseReasonForSecurityController.show))
    ) { firstDepositId =>
      if (claim.isSingleSecurity) {
        claim
          .selectSecurityDepositId(firstDepositId)
          .fold(
            error => {
              logger.error(s"Error selecting security deposit - $error")
              (claim, errorHandler.errorResult())
            },
            updatedClaim => (updatedClaim, Redirect(routes.CheckDeclarationDetailsSingleSecurityController.show))
          )
      } else {
        (
          claim,
          Redirect(routes.SelectSecuritiesController.show(firstDepositId))
        )
      }
    }
  }

  final def show(securityDepositId: String): Action[AnyContent] = actionReadClaim { claim =>
    val postAction: Call = routes.SelectSecuritiesController.submit(securityDepositId)
    claim
      .getDisplayDeclarationIfValidSecurityDepositId(securityDepositId)
      .fold(Redirect(baseRoutes.IneligibleController.ineligible)) { declaration =>
        Ok(
          selectSecuritiesPage(
            form.withDefault(claim.getSecuritySelectionStatus(securityDepositId)),
            declaration,
            securityDepositId,
            postAction
          )
        )
      }

  }

  final def submit(securityDepositId: String): Action[AnyContent] = actionReadWriteClaim { claim =>
    val postAction: Call = routes.SelectSecuritiesController.submit(securityDepositId)
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            claim,
            claim
              .getDisplayDeclarationIfValidSecurityDepositId(securityDepositId)
              .map(declaration =>
                BadRequest(selectSecuritiesPage(formWithErrors, declaration, securityDepositId, postAction))
              )
              .getOrElse(errorHandler.errorResult())
          ),
        yesno =>
          yesno match {
            case YesNo.Yes =>
              claim
                .selectSecurityDepositId(securityDepositId)
                .fold(
                  error => {
                    logger.error(s"Error selecting security deposit - $error")
                    (claim, Redirect(routes.SelectSecuritiesController.show(securityDepositId)))
                  },
                  updatedClaim => (updatedClaim, Redirect(nextPage(claim, securityDepositId)))
                )

            case YesNo.No =>
              claim
                .removeSecurityDepositId(securityDepositId)
                .fold(
                  error => {
                    logger.error(s"Error de-selecting security deposit - $error")
                    (claim, Redirect(routes.SelectSecuritiesController.show(securityDepositId)))
                  },
                  updatedClaim => (updatedClaim, Redirect(nextPage(claim, securityDepositId)))
                )
          }
      )
  }

  private def nextPage(claim: SecuritiesClaim, securityDepositId: String): Call =
    if claim.userHasSeenCYAPage then routes.CheckYourAnswersController.show
    else if claim.answers.modes.checkDeclarationDetailsChangeMode then routes.CheckDeclarationDetailsController.show
    else
      claim.getSecurityDepositIds
        .nextAfter(securityDepositId)
        .fold(routes.CheckDeclarationDetailsController.show)(routes.SelectSecuritiesController.show(_))

}
