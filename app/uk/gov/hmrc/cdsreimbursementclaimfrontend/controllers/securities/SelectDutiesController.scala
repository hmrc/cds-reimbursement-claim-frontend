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
import play.api.i18n.Messages
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error as CdsError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.select_duties
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectDutiesController @Inject() (
  val jcc: JourneyControllerComponents,
  val featureSwitchService: FeatureSwitchService,
  selectDutiesPage: select_duties
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  private def processAvailableDuties[T](
    securityId: String,
    journey: SecuritiesJourney,
    error: CdsError => Future[T],
    f: Seq[DutyAmount] => Future[T]
  ): Future[T] =
    journey
      .getSecurityTaxCodesWithAmounts(securityId)
      .noneIfEmpty
      .fold(error(CdsError("no tax codes available")))(f)

  final val showFirst: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.getSecurityDepositIds.headOption
      .map { securityId =>
        processAvailableDuties[Result](
          securityId: String,
          journey: SecuritiesJourney,
          error => {
            logger.warn(s"No Available duties: $error")
            Redirect(baseRoutes.IneligibleController.ineligible).asFuture
          },
          dutiesAvailable =>
            {
              val emptyForm: Form[Seq[TaxCode]] = selectDutiesForm(dutiesAvailable.map(_.taxCode))

              val filledForm =
                emptyForm.withDefault(journey.getSelectedDutiesFor(securityId))

              Ok(
                selectDutiesPage(
                  filledForm,
                  journey.isSingleSecurity,
                  securityId,
                  dutiesAvailable,
                  routes.SelectDutiesController.submit(securityId)
                )
              )
            }.asFuture
        )
      }
      .getOrElse {
        logger.warn(s"Cannot find any security deposit")
        Redirect(baseRoutes.IneligibleController.ineligible).asFuture
      }
  }

  final def show(securityId: String): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    processAvailableDuties[Result](
      securityId: String,
      journey: SecuritiesJourney,
      error => {
        logger.warn(s"No Available duties: $error")
        Redirect(baseRoutes.IneligibleController.ineligible).asFuture
      },
      dutiesAvailable =>
        {
          val emptyForm: Form[Seq[TaxCode]] = selectDutiesForm(dutiesAvailable.map(_.taxCode))

          val filledForm =
            emptyForm.withDefault(journey.getSelectedDutiesFor(securityId))

          Ok(
            selectDutiesPage(
              filledForm,
              journey.isSingleSecurity,
              securityId,
              dutiesAvailable,
              routes.SelectDutiesController.submit(securityId)
            )
          )
        }.asFuture
    )
  }

  final def submit(securityId: String): Action[AnyContent] = actionReadWriteJourney(
    implicit request =>
      journey =>
        processAvailableDuties[(SecuritiesJourney, Result)](
          securityId: String,
          journey: SecuritiesJourney,
          error => {
            logger.warn(s"No Available duties: $error")
            (journey, Redirect(baseRoutes.IneligibleController.ineligible)).asFuture
          },
          dutiesAvailable => {
            val form      = selectDutiesForm(dutiesAvailable.map(_.taxCode))
            val boundForm = form.bindFromRequest()
            boundForm
              .fold(
                errors => {
                  logger.warn(
                    s"Selection of duties to be repaid failed for $securityId because of errors:" +
                      s"${errors.errors.mkString("", ",", "")}"
                  )
                  (
                    journey,
                    Ok(
                      selectDutiesPage(
                        boundForm,
                        journey.isSingleSecurity,
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
        ),
    fastForwardToCYAEnabled = false
  )

  private def updateAndRedirect(
    journey: SecuritiesJourney,
    securityId: String,
    dutiesSelected: Seq[TaxCode]
  )(using HeaderCarrier): (SecuritiesJourney, Result) =
    if journey
        .getSelectedDutiesFor(securityId)
        .containsSameElements(dutiesSelected) && journey.userHasSeenCYAPage
    then (journey, Redirect(checkYourAnswers))
    else
      journey
        .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, dutiesSelected)
        .fold(
          error => {
            logger.warn(error)
            (journey, Redirect(controllers.routes.IneligibleController.ineligible))
          },
          updatedJourney =>
            (
              updatedJourney,
              Redirect(
                if updatedJourney.answers.modes.checkClaimDetailsChangeMode && updatedJourney.answers.modes.claimFullAmountMode
                then
                  journey.getNextDepositIdAndTaxCodeToClaim match {
                    case Some(Left(depositId)) =>
                      routes.ConfirmFullRepaymentController.show(depositId)

                    case Some(Right((depositId, taxCode))) =>
                      routes.EnterClaimController.show(depositId, taxCode)

                    case None =>
                      if journey.isSingleSecurity && featureSwitchService.isEnabled(Feature.SingleSecurityTrack) then
                        routes.CheckClaimDetailsSingleSecurityController.show
                      else routes.CheckClaimDetailsController.show
                  }
                else routes.EnterClaimController.showFirst(securityId)
              )
            )
        )
}
object SelectDutiesController extends Logging {
  val selectDutiesKey: String = "select-duties"

  def getDescription(fullKey: String, messages: Messages): Option[String] =
    if messages.isDefinedAt(fullKey) then Some(messages(fullKey))
    else {
      logger.warn(s"no description found for $fullKey")
      None
    }
}
