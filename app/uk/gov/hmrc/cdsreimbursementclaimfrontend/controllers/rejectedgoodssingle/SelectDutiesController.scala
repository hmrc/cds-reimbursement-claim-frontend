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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.select_duties

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectDutiesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectDutiesPage: select_duties
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  val postAction: Call = routes.SelectDutiesController.submit()

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val availableDuties: Seq[(TaxCode, Boolean)] = journey.getAvailableDuties
    val h1MessageKey                             =
      if (journey.getLeadDisplayDeclaration.exists(_.hasOnlySubsidyPayments))
        Some("select-duties.subsidies.title")
      else None

    if (availableDuties.isEmpty) {
      logger.warn("No available duties")
      Redirect(baseRoutes.IneligibleController.ineligible())
    } else {
      val form = selectDutiesForm(availableDuties.map(_._1)).withDefault(journey.getSelectedDuties)
      Ok(selectDutiesPage(form, availableDuties, None, true, None, None, postAction, h1MessageKey))
    }
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      val availableDuties: Seq[(TaxCode, Boolean)] = journey.getAvailableDuties
      Future.successful(if (availableDuties.isEmpty) {
        logger.warn("No available duties")
        (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
      } else {
        val form = selectDutiesForm(availableDuties.map(_._1))
        form
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                journey,
                BadRequest(selectDutiesPage(formWithErrors, availableDuties, None, true, None, None, postAction))
              ),
            taxCodesSelected =>
              (
                journey
                  .selectAndReplaceTaxCodeSetForReimbursement(taxCodesSelected)
                  .getOrElse(journey),
                Redirect(routes.EnterClaimController.showFirst)
              )
          )
      })
    },
    fastForwardToCYAEnabled = false
  )
}

object SelectDutiesController {
  val selectDutiesKey: String = "select-duties"
}
