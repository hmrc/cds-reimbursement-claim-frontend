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

import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectSecuritiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.select_securities

import scala.concurrent.ExecutionContext

@Singleton
class SelectSecuritiesController @Inject() (
  val jcc: JourneyControllerComponents,
  val featureSwitchService: FeatureSwitchService,
  selectSecuritiesPage: select_securities
)(implicit val viewConfig: ViewConfig, errorHandler: ErrorHandler, val ec: ExecutionContext)
    extends SecuritiesJourneyBaseController
    with SecuritiesJourneyRouter
    with Logging {

  private val form: Form[YesNo] = selectSecuritiesForm

  import SecuritiesJourney.Checks._

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final val showFirst: Action[AnyContent] = simpleActionReadWriteJourney { implicit request => journey =>
    journey.getSecurityDepositIds.headOption.fold(
      (journey, Redirect(routes.ChooseReasonForSecurityController.show))
    ) { firstDepositId =>
      if (journey.isSingleSecurity && featureSwitchService.isEnabled(Feature.SingleSecurityTrack)) {
        journey
          .selectSecurityDepositId(firstDepositId)
          .fold(
            error => {
              logger.error(s"Error selecting security deposit - $error")
              (journey, errorHandler.errorResult())
            },
            updatedJourney => (updatedJourney, Redirect(routes.CheckDeclarationDetailsSingleSecurityController.show))
          )
      } else {
        (
          journey,
          Redirect(routes.SelectSecuritiesController.show(firstDepositId))
        )
      }
    }
  }

  final def show(securityDepositId: String): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val postAction: Call = routes.SelectSecuritiesController.submit(securityDepositId)
    journey
      .getDisplayDeclarationIfValidSecurityDepositId(securityDepositId)
      .fold(Redirect(baseRoutes.IneligibleController.ineligible)) { declaration =>
        Ok(
          selectSecuritiesPage(
            form.withDefault(journey.getSecuritySelectionStatus(securityDepositId)),
            declaration,
            securityDepositId,
            postAction
          )
        )
      }
      .asFuture
  }

  final def submit(securityDepositId: String): Action[AnyContent] = actionReadWriteJourney {
    implicit request => journey =>
      val postAction: Call = routes.SelectSecuritiesController.submit(securityDepositId)
      form
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              journey
                .getDisplayDeclarationIfValidSecurityDepositId(securityDepositId)
                .map(declaration =>
                  BadRequest(selectSecuritiesPage(formWithErrors, declaration, securityDepositId, postAction))
                )
                .getOrElse(errorHandler.errorResult())
            ).asFuture,
          yesno =>
            (yesno match {
              case YesNo.Yes =>
                journey
                  .selectSecurityDepositId(securityDepositId)
                  .fold(
                    error => {
                      logger.error(s"Error selecting security deposit - $error")
                      (journey, Redirect(routes.SelectSecuritiesController.show(securityDepositId)))
                    },
                    updatedJourney => (updatedJourney, Redirect(nextPage(journey, securityDepositId)))
                  )

              case YesNo.No =>
                journey
                  .removeSecurityDepositId(securityDepositId)
                  .fold(
                    error => {
                      logger.error(s"Error de-selecting security deposit - $error")
                      (journey, Redirect(routes.SelectSecuritiesController.show(securityDepositId)))
                    },
                    updatedJourney => (updatedJourney, Redirect(nextPage(journey, securityDepositId)))
                  )
            }).asFuture
        )
  }

  private def nextPage(journey: SecuritiesJourney, securityDepositId: String): Call =
    if journey.userHasSeenCYAPage then routes.CheckYourAnswersController.show
    else if journey.answers.modes.checkDeclarationDetailsChangeMode then routes.CheckDeclarationDetailsController.show
    else
      journey.getSecurityDepositIds
        .nextAfter(securityDepositId)
        .fold(routes.CheckDeclarationDetailsController.show)(routes.SelectSecuritiesController.show(_))

}
