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

import cats.data._
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.i18n.Messages
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Result
import shapeless.syntax.std.tuple.productTupleOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error => CdsError}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutiesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectDutiesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectDutiesPage: securities.select_duties // todo check SecurityId display / clone page or
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  private def processAvailableDuties[T](
    securityId: String,
    journey: SecuritiesJourney,
    error: CdsError => Future[T],
    f: NonEmptyList[DutyAmount] => Future[T]
  ): Future[T] = {
    val taxCodesAndValuesAvailable = journey
      .getSecurityTaxCodesFor(securityId)
      .flatMap(journey.getSecurityTaxDetailsFor(securityId, _).toList)
      .toList
    NonEmptyList
      .fromList(taxCodesAndValuesAvailable.map(x => DutyAmount(x.getTaxCode, x.getAmount)))
      .fold(error(CdsError("no tax codes available")))(f)
  }

  final def show(securityId: String): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    processAvailableDuties[Result](
      securityId: String,
      journey: SecuritiesJourney,
      error => {
        logger.warn(s"No Available duties: $error")
        Redirect(baseRoutes.IneligibleController.ineligible()).asFuture
      },
      dutiesAvailable =>
        {
          val emptyForm: Form[DutiesSelectedAnswer] = selectDutiesForm(dutiesAvailable.map(_.asDuty))

          val filledForm =
            emptyForm.withDefault(
              journey.getSelectedDutiesFor(securityId).flatMap(_.nonEmptyList.map(_.map(Duty.apply)))
            )

          Ok(
            selectDutiesPage(filledForm, securityId, dutiesAvailable, routes.SelectDutiesController.submit(securityId))
          )
        }.asFuture
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final def submit(securityId: String): Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      processAvailableDuties[(SecuritiesJourney, Result)](
        securityId: String,
        journey: SecuritiesJourney,
        error => {
          logger.warn(s"No Available duties: $error")
          (journey, Redirect(baseRoutes.IneligibleController.ineligible())).asFuture
        },
        dutiesAvailable => {
          val form      = selectDutiesForm(dutiesAvailable.map(_.asDuty))
          val boundForm = form.bindFromRequest()
          boundForm
            .fold(
              errors => {
                logger.warn(
                  s"Selection of duties to be repaid failed for $securityId because of errors:" +
                    s"${errors.mkString("", ",", "")}"
                )
                (
                  journey,
                  Ok(
                    selectDutiesPage(
                      boundForm,
                      securityId,
                      dutiesAvailable,
                      routes.SelectDutiesController.submit(securityId)
                    )
                  )
                )
              },
              dutiesSelected => updateAndRedirect(journey, securityId, dutiesSelected)
            )
            .asFuture
        }
      )
    },
    fastForwardToCYAEnabled = false
  )

  private def updateAndRedirect(
    journey: SecuritiesJourney,
    securityId: String,
    dutiesSelected: DutiesSelectedAnswer
  ): (SecuritiesJourney, Result) =
    journey
      .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, dutiesSelected.map(_.taxCode).toList)
      .fold(
        error => {
          logger.warn(error)
          (journey, Redirect(controllers.routes.IneligibleController.ineligible()))
        },
        updatedJourney =>
          (
            updatedJourney,
            Redirect(
              if (updatedJourney.answers.checkClaimDetailsChangeMode && updatedJourney.answers.claimFullAmountMode)
                journey.getNextDepositIdAndTaxCodeToClaim match {
                  case Some(Left(depositId)) =>
                    routes.ConfirmFullRepaymentController.show(depositId)

                  case Some(Right((depositId, taxCode))) =>
                    routes.EnterClaimController.show(depositId, taxCode)

                  case None =>
                    routes.CheckClaimDetailsController.show()
                }
              else
                routes.EnterClaimController.show(securityId, dutiesSelected.head.taxCode)
            )
          )
      )
}
object SelectDutiesController extends Logging {
  val selectDutiesKey: String = "select-duties"

  def getDescription(fullKey: String, messages: Messages): Option[String] =
    if (messages.isDefinedAt(fullKey))
      Some(messages(fullKey))
    else {
      logger.warn(s"no description found for $fullKey")
      None
    }
}
