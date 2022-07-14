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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import cats.implicits._
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.{Action, AnyContent, Call, Request, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.confirmFullRepaymentForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.WorkInProgressMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.{No, Yes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.confirm_full_repayment

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConfirmFullRepaymentController @Inject() (
  val jcc: JourneyControllerComponents,
  confirmFullRepaymentPage: confirm_full_repayment
)(implicit viewConfig: ViewConfig, errorHandler: ErrorHandler, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController
    with SecuritiesJourneyRouter
    with Logging {

  private val form: Form[YesNo] = confirmFullRepaymentForm

  // todo import SecuritiesJourney.Checks._

  // GET          /securities/confirm-full-repayment/:id
  // @uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.ConfirmFullRepaymentController.show(id: String)
  // @(form: Form[YesNo], securityId: String, totalValue: BigDecimal, postAction: Call)
  //    (implicit request: RequestWithSessionData[_], messages: Messages, viewConfig: ViewConfig)
  def show(id: String): Action[AnyContent] = actionReadJourney { implicit request =>
    journey =>
      journey
        .getDisplayDeclarationIfValidSecurityDepositId(id)
        .fold(errorHandler.errorResult()) { declaration =>
          Ok(
            confirmFullRepaymentPage(
              form.withDefault(
                journey.answers.reclaimingFullAmount
                  .fold(Option.empty[YesNo])(_ => Some(YesNo.Yes))
              ),
              id,
              journey.getTotalReclaimAmount,
              routes.ConfirmFullRepaymentController.submit(id)
            )
          )
        }
        .asFuture
  }

  // POST         /securities/confirm-full-repayment/:id
  // @uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.ConfirmFullRepaymentController.submit(id: String)
  def submit(id: String): Action[AnyContent] = actionReadWriteJourney { implicit request =>
    journey =>
      form.bindFromRequest
        .fold(
          formWithErrors =>
            (
              journey,
              journey
                .getDisplayDeclarationIfValidSecurityDepositId(id)
                .map(declaration =>
                  BadRequest(
                    confirmFullRepaymentPage(
                      formWithErrors, id, journey.getTotalReclaimAmount, routes.ConfirmFullRepaymentController.submit(id)
                    )
                  )
                )
                .getOrElse(errorHandler.errorResult())
              ).asFuture,
          {
            case Yes =>
              submitYes(id, journey)
            case No =>
              (journey, Redirect(routes.ConfirmFullRepaymentController.submit(id))).asFuture
          }
        )
  }

  def submitYes(securityId: String, journey: SecuritiesJourney)
               (implicit request: Request[_]): Future[(SecuritiesJourney, Result)] = {

    journey.submitFullAmountsForReclaim(securityId)
      .fold({ error =>
        logger.warn(error)
        (journey, errorHandler.errorResult())
      },
        updatedJourney =>
          (updatedJourney, Redirect(routes.ChooseFileTypeController.submit()))
      ).asFuture
  }
}
