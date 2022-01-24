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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Cookie
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  enterClaim: pages.enter_claim
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  val key: String               = "enter-claim.rejected-goods.single"
  val taxCodeCookieName: String = "taxCode"
  val postAction: Call          = routes.EnterClaimController.submit()

  def show(): Action[AnyContent] = showInner(None)

  def showAmend(taxCode: TaxCode): Action[AnyContent] = showInner(Some(taxCode))

  def showInner(taxCode: Option[TaxCode]): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    (
      taxCode,
      taxCode.flatMap(code => journey.getNdrcDetailsFor(code)),
      journey.getNextNdrcDetailsToClaim
    ) match {
      case (_, Some(_), Some(_))                                                        =>
        Redirect(routes.EnterClaimController.show()).asFuture
      case (_, _, Some(ndrcDetails))                                                    =>
        displayFormFor(ndrcDetails)
      case (Some(code), Some(ndrcDetails), _) if journey.hasCompleteReimbursementClaims =>
        displayFormFor(ndrcDetails, journey.getReimbursementClaims.get(code))
      case (_, _, None) if journey.hasCompleteReimbursementClaims                       =>
        Redirect(routes.CheckClaimDetailsController.show()).asFuture
      case (_, _, None)                                                                 =>
        Redirect(routes.SelectTaxCodesController.show()).asFuture
    }
  }

  private def displayFormFor(ndrcDetails: NdrcDetails, claimedAmount: Option[BigDecimal] = None)(implicit
    request: Request[_]
  ): Future[Result] = {
    val amountPaid = BigDecimal(ndrcDetails.amount)
    val form       = Forms.claimAmountForm(key, amountPaid).withDefault(claimedAmount)
    Ok(enterClaim(form, TaxCode(ndrcDetails.taxType), amountPaid, postAction))
      .withCookies(Cookie(taxCodeCookieName, ndrcDetails.taxType))
      .asFuture
  }

  def submit(): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    request.cookies.get(taxCodeCookieName) match {
      case Some(Cookie(_, taxCode, _, _, _, _, _, _)) =>
        journey.getNdrcDetailsFor(TaxCode(taxCode)) match {
          case Some(ndrcDetails) =>
            val form = Forms.claimAmountForm(key, BigDecimal(ndrcDetails.amount))
            form
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  Future.successful(
                    (
                      journey,
                      BadRequest(
                        enterClaim(
                          formWithErrors,
                          TaxCode(ndrcDetails.taxType),
                          BigDecimal(ndrcDetails.amount),
                          postAction
                        )
                      )
                    )
                  ),
                reimbursementAmount =>
                  Future.successful(
                    journey
                      .submitAmountForReimbursement(TaxCode(taxCode), reimbursementAmount)
                      .fold(
                        error => {
                          logger.error(s"Error submitting reimbursement claim amount - $error")
                          (journey, Redirect(routes.EnterClaimController.show()))
                        },
                        updatedJourney =>
                          (
                            updatedJourney,
                            if (updatedJourney.hasCompleteReimbursementClaims)
                              Redirect(routes.CheckClaimDetailsController.show())
                            else
                              Redirect(routes.EnterClaimController.show())
                          )
                      )
                  )
              )
          case None              =>
            logger.error(s"Attempting to claim a reimbursement before selecting an MRN")
            Future.successful((journey, Redirect(routes.EnterMovementReferenceNumberController.show())))
        }
      case None                                       =>
        Future.successful(
          (journey, Redirect(routes.EnterClaimController.show()))
        )
    }
  }
}
