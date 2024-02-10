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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import com.github.arturopala.validator.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_scheduled_claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterScheduledClaimMixin

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  val enterClaimPage: enter_scheduled_claim
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsScheduledJourneyBaseController
    with EnterScheduledClaimMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  override val routesPack = EnterClaimController.routesPack

  final def modifyJourney(
    journey: OverpaymentsScheduledJourney,
    dutyType: DutyType,
    taxCode: TaxCode,
    paidAmount: BigDecimal,
    correctAmount: BigDecimal
  ): Either[String, Journey] =
    journey
      .submitCorrectAmount(
        dutyType,
        taxCode,
        paidAmount,
        correctAmount
      )
}

object EnterClaimController {
  val routesPack = EnterScheduledClaimMixin.RoutesPack(
    showAction = routes.EnterClaimController.show,
    postAction = routes.EnterClaimController.submit,
    showSelectDutyTypes = routes.SelectDutyTypesController.show,
    showSelectDuties = routes.SelectDutiesController.show,
    showCheckClaimDetails = routes.CheckClaimDetailsController.show
  )
}
