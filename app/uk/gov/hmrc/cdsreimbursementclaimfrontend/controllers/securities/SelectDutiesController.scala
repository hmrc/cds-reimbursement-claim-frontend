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
import play.api.i18n.Messages
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.hasMRNAndDisplayDeclarationAndRfS
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error as CdsError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.select_duties
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectDutiesController @Inject() (
  val jcc: ClaimControllerComponents,
  selectDutiesPage: select_duties
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesClaimBaseController {

  final override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  private def processAvailableDuties[J, T](
    securityId: String,
    claim: SecuritiesClaim,
    error: CdsError => Future[(J, T)],
    f: Seq[DutyAmount] => Future[(J, T)]
  ): Future[(J, T)] =
    claim
      .getSecurityTaxCodesWithAmounts(securityId)
      .noneIfEmpty
      .fold(error(CdsError("no tax codes available")))(f)

  final val showFirst: Action[AnyContent] = actionReadWriteClaim { implicit request => claim =>
    claim.getSecurityDepositIds.headOption
      .map { securityId =>
        processAvailableDuties[SecuritiesClaim, Result](
          securityId: String,
          claim: SecuritiesClaim,
          error => {
            logger.warn(s"No Available duties: $error")
            (claim, Redirect(baseRoutes.IneligibleController.ineligible)).asFuture
          },
          dutiesAvailable =>
            {
              dutiesAvailable.toList match
                case duty :: Nil =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, Seq(duty.taxCode))
                    .fold(
                      error => throw new Exception(error),
                      (_, Redirect(routes.EnterClaimController.showFirst(securityId)))
                    )

                case duties =>
                  val emptyForm: Form[Seq[TaxCode]] = selectDutiesForm(dutiesAvailable.map(_.taxCode))

                  val filledForm =
                    emptyForm.withDefault(claim.getSelectedDutiesFor(securityId))

                  (
                    claim,
                    Ok(
                      selectDutiesPage(
                        filledForm,
                        claim.isSingleSecurity,
                        securityId,
                        dutiesAvailable,
                        routes.SelectDutiesController.submit(securityId)
                      )
                    )
                  )
            }.asFuture
        )
      }
      .getOrElse {
        logger.warn(s"Cannot find any security deposit")
        (claim, Redirect(baseRoutes.IneligibleController.ineligible)).asFuture
      }
  }

  final def show(securityId: String): Action[AnyContent] = actionReadWriteClaim { implicit request => claim =>
    processAvailableDuties[SecuritiesClaim, Result](
      securityId: String,
      claim: SecuritiesClaim,
      error => {
        logger.warn(s"No Available duties: $error")
        (claim, Redirect(baseRoutes.IneligibleController.ineligible)).asFuture
      },
      dutiesAvailable =>
        {
          val emptyForm: Form[Seq[TaxCode]] = selectDutiesForm(dutiesAvailable.map(_.taxCode))

          val filledForm =
            emptyForm.withDefault(claim.getSelectedDutiesFor(securityId))

          (
            claim,
            Ok(
              selectDutiesPage(
                filledForm,
                claim.isSingleSecurity,
                securityId,
                dutiesAvailable,
                routes.SelectDutiesController.submit(securityId)
              )
            )
          )
        }.asFuture
    )
  }

  final def submit(securityId: String): Action[AnyContent] = actionReadWriteClaim(
    implicit request =>
      claim =>
        processAvailableDuties[SecuritiesClaim, Result](
          securityId: String,
          claim: SecuritiesClaim,
          error => {
            logger.warn(s"No Available duties: $error")
            (claim, Redirect(baseRoutes.IneligibleController.ineligible)).asFuture
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
                    claim,
                    Ok(
                      selectDutiesPage(
                        boundForm,
                        claim.isSingleSecurity,
                        securityId,
                        dutiesAvailable,
                        routes.SelectDutiesController.submit(securityId)
                      )
                    )
                  )
                },
                dutiesSelected => updateAndRedirect(claim, securityId, dutiesSelected)
              )
              .asFuture
          }
        ),
    fastForwardToCYAEnabled = false
  )

  private def updateAndRedirect(
    claim: SecuritiesClaim,
    securityId: String,
    dutiesSelected: Seq[TaxCode]
  )(using HeaderCarrier): (SecuritiesClaim, Result) =
    if claim
        .getSelectedDutiesFor(securityId)
        .containsSameElements(dutiesSelected) && claim.userHasSeenCYAPage
    then (claim, Redirect(checkYourAnswers))
    else
      claim
        .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, dutiesSelected)
        .fold(
          error => {
            logger.warn(error)
            (claim, Redirect(controllers.routes.IneligibleController.ineligible))
          },
          updatedClaim =>
            (
              updatedClaim,
              Redirect(
                if updatedClaim.answers.modes.checkClaimDetailsChangeMode && updatedClaim.answers.modes.claimFullAmountMode
                then
                  claim.getNextDepositIdAndTaxCodeToClaim match {
                    case Some(Left(depositId)) =>
                      if claim.isSingleSecurity then routes.ConfirmSingleDepositRepaymentController.show
                      else routes.ConfirmFullRepaymentController.show(depositId)

                    case Some(Right((depositId, taxCode))) =>
                      routes.EnterClaimController.show(depositId, taxCode)

                    case None =>
                      if claim.isSingleSecurity then routes.CheckClaimDetailsSingleSecurityController.show
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
