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
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.confirmFullRepaymentForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.WorkInProgressMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.confirm_full_repayment

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try

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

  // GET          /securities/confirm-full-repayment
  // @uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.ConfirmFullRepaymentController.showFirst()
  // @(form: Form[YesNo], securityId: String, totalValue: BigDecimal, postAction: Call)
  //    (implicit request: RequestWithSessionData[_], messages: Messages, viewConfig: ViewConfig)
  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def showFirst(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.getSecuritiesReclaims.headOption.fold(
      errorHandler.errorResult().asFuture
    )(reclaims => showForId(request, journey, reclaims._1))
  }

  // GET          /securities/confirm-full-repayment/:id
  // @uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.ConfirmFullRepaymentController.show(id: String)
  // @(form: Form[YesNo], securityId: String, totalValue: BigDecimal, postAction: Call)
  //    (implicit request: RequestWithSessionData[_], messages: Messages, viewConfig: ViewConfig)
  def show(id: String): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    showForId(request, journey, id)
  }

  private def showForId(implicit request: Request[_], journey: SecuritiesJourney, id: String) =
    journey
      .getDisplayDeclarationIfValidSecurityDepositId(id)
      .flatMap(_.getSecurityDetailsFor(id).map(_.amountPaid))
      .flatMap(x => Try(BigDecimal(x)).toOption)
      .fold(errorHandler.errorResult()) { amountPaid =>
        Ok(
          confirmFullRepaymentPage(
            form.withDefault(
              journey.answers.reclaimingFullAmount
                .fold(Option.empty[YesNo])(_ => Some(YesNo.Yes))
            ),
            id,
            amountPaid,
            routes.ConfirmFullRepaymentController.submit(id)
          )
        )
      }
      .asFuture

  // POST         /securities/confirm-full-repayment/:id
  // @uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.ConfirmFullRepaymentController.submit(id: String)
  def submit(id: String): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    form.bindFromRequest
      .fold(
        formWithErrors =>
          (
            journey,
            journey
              .getDisplayDeclarationIfValidSecurityDepositId(id)
              .flatMap(_.getSecurityDetailsFor(id).map(_.amountPaid))
              .flatMap(x => Try(BigDecimal(x)).toOption)
              .map(amountPaid =>
                BadRequest(
                  confirmFullRepaymentPage(
                    formWithErrors,
                    id,
                    amountPaid,
                    routes.ConfirmFullRepaymentController.submit(id)
                  )
                )
              )
              .getOrElse(errorHandler.errorResult())
          ).asFuture,
        {
          case Yes =>
            submitYes(id, journey)
          case No  =>
            submitNo(id, journey)
        }
      )
  }

  def submitYes(securityId: String, journey: SecuritiesJourney)(implicit
    request: Request[_]
  ): Future[(SecuritiesJourney, Result)] =
    journey
      .submitFullAmountsForReclaim(securityId)
      .fold(
        { error =>
          logger.warn(error)
          (journey, errorHandler.errorResult())
        },
        updatedJourney => (updatedJourney, Redirect(routes.ChooseFileTypeController.submit()))
      )
      .asFuture

  def submitNo(securityId: String, journey: SecuritiesJourney)(implicit
    request: Request[_]
  ): Future[(SecuritiesJourney, Result)] =
    journey
      .clearReclaimAmount(securityId)
      .fold(
        { error =>
          logger.warn(error)
          (journey, errorHandler.errorResult())
        },
        updatedJourney => (updatedJourney, Redirect(routes.SelectDutiesController.submit(securityId)))
      )
      .asFuture
}
