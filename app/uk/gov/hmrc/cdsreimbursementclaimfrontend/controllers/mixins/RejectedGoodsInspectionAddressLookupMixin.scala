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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.inspectionAddressTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.HaveInspectionDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsClaimProperties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Declarant
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Importer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Other
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.choose_inspection_address_type

trait RejectedGoodsInspectionAddressLookupMixin extends ClaimBaseController with AddressLookupMixin {

  type Claim <: claims.Claim & ClaimBase & RejectedGoodsClaimProperties & HaveInspectionDetails

  val startAddressLookup: Call
  val postAction: Call

  val inspectionAddressPage: choose_inspection_address_type

  def modifyClaim(claim: Claim, inspectionAddress: InspectionAddress): Claim

  final val show: Action[AnyContent] = actionReadClaim { implicit request => claim =>
    claim.getPotentialInspectionAddresses match {
      case Nil =>
        Redirect(startAddressLookup)
      case xs  =>
        Ok(
          inspectionAddressPage(
            xs,
            inspectionAddressTypeForm.withDefault(claim.getInspectionAddressType),
            postAction
          )
        )
    }
  }

  final val submit: Action[AnyContent] = actionReadWriteClaim(
    implicit request =>
      claim =>
        inspectionAddressTypeForm
          .bindFromRequest()
          .fold(
            errors =>
              (
                claim,
                BadRequest(inspectionAddressPage(claim.getPotentialInspectionAddresses, errors, postAction))
              ),
            {
              case Other     =>
                (claim, Redirect(startAddressLookup))
              case Declarant =>
                claim.getDeclarantContactDetailsFromACC14
                  .map { address =>
                    redirectToTheNextPage(modifyClaim(claim, InspectionAddress.ofType(Declarant).mapFrom(address)))
                  }
                  .getOrElse((claim, Redirect(baseRoutes.IneligibleController.ineligible)))

              case Importer =>
                claim.getConsigneeContactDetailsFromACC14
                  .map { address =>
                    redirectToTheNextPage(modifyClaim(claim, InspectionAddress.ofType(Importer).mapFrom(address)))
                  }
                  .getOrElse((claim, Redirect(baseRoutes.IneligibleController.ineligible)))

            }
          ),
    fastForwardToCYAEnabled = false
  )

}
