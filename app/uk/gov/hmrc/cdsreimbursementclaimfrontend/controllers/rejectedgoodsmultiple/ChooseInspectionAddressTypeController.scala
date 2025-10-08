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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.RejectedGoodsInspectionAddressLookupMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.choose_inspection_address_type

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class ChooseInspectionAddressTypeController @Inject() (
  val jcc: ClaimControllerComponents,
  val addressLookupService: AddressLookupService,
  val inspectionAddressPage: choose_inspection_address_type
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends RejectedGoodsMultipleClaimBaseController
    with RejectedGoodsInspectionAddressLookupMixin {

  private val nextPage: Call =
    routes.ChoosePayeeTypeController.show

  override val postAction: Call =
    routes.ChooseInspectionAddressTypeController.submit

  override val problemWithAddressPage: Call =
    routes.ProblemWithAddressController.show

  override val startAddressLookup: Call =
    routes.ChooseInspectionAddressTypeController.redirectToALF()

  override val retrieveLookupAddress: Call =
    routes.ChooseInspectionAddressTypeController.retrieveAddressFromALF()

  final override def modifyClaim(claim: Claim, inspectionAddress: InspectionAddress): Claim =
    claim.submitInspectionAddress(inspectionAddress)

  final override def modifyClaim(claim: Claim, contactAddress: ContactAddress): Claim =
    claim.submitInspectionAddress(InspectionAddress.ofType(Other).mapFrom(contactAddress))

  override def redirectToTheNextPage(claim: RejectedGoodsMultipleClaim): (RejectedGoodsMultipleClaim, Result) =
    (
      claim,
      if claim.hasCompleteAnswers then Redirect(checkYourAnswers)
      else Redirect(nextPage)
    )
}
