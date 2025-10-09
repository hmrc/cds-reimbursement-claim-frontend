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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterScheduledClaimMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_scheduled_claim

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterClaimController @Inject() (
  val jcc: ClaimControllerComponents,
  val enterClaimPage: enter_scheduled_claim
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledClaimBaseController
    with EnterScheduledClaimMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledClaim]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  override val routesPack = EnterClaimController.routesPack

  final def modifyClaim(
    claim: RejectedGoodsScheduledClaim,
    dutyType: DutyType,
    taxCode: TaxCode,
    paidAmount: BigDecimal,
    claimAmount: BigDecimal
  ): Either[String, Claim] =
    claim
      .submitClaimAmount(
        dutyType,
        taxCode,
        paidAmount,
        claimAmount
      )
}

object EnterClaimController {
  val routesPack = EnterScheduledClaimMixin.RoutesPack(
    showAction = routes.EnterClaimController.show,
    postAction = routes.EnterClaimController.submit,
    showSelectDutyTypes = routes.SelectDutyTypesController.show,
    showSelectDuties = routes.SelectDutiesController.show,
    showSelectExciseCategoryDuties = routes.SelectDutiesController.showExciseDuties,
    showCheckClaimDetails = routes.CheckClaimDetailsController.show
  )
}
