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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  enterClaim: pages.enter_claim
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  val key               = "enter-claim.rejected-goods.single"
  val taxCodeCookieName = "taxCode"
  val postAction: Call  = routes.EnterClaimController.submit()

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.getNextNdrcDetailsToClaim match {
      case Some(ndrcDetails)                              =>
        val amountPaid = BigDecimal(ndrcDetails.amount)
        val form       = Forms.claimAmountForm(ndrcDetails.taxType, amountPaid)
        Future.successful(
          Ok(enterClaim(form, TaxCode(ndrcDetails.taxType), amountPaid, postAction))
            .withCookies(Cookie(taxCodeCookieName, ndrcDetails.taxType))
        )
      case None if journey.hasCompleteReimbursementClaims => Redirect("total_reimbursement").asFuture
      case None                                           => Redirect(routes.SelectTaxCodesController.show()).asFuture
    }
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
                              Redirect("total_reimbursement") //TODO: Set the correct details
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
