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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.HaveInspectionDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsJourneyProperties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Declarant
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Importer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Other
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

trait RejectedGoodsInspectionAddressLookupMixin extends JourneyBaseController with AddressLookupMixin {

  type Journey <: journeys.Journey with JourneyBase with RejectedGoodsJourneyProperties with HaveInspectionDetails

  val startAddressLookup: Call
  val postAction: Call

  val inspectionAddressPage: pages.choose_inspection_address_type

  def modifyJourney(journey: Journey, inspectionAddress: InspectionAddress): Journey

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.getPotentialInspectionAddresses match {
      case Nil =>
        Redirect(startAddressLookup).asFuture
      case xs  =>
        Ok(
          inspectionAddressPage(
            xs,
            inspectionAddressTypeForm.withDefault(journey.getInspectionAddressType),
            postAction
          )
        ).asFuture
    }
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      inspectionAddressTypeForm
        .bindFromRequest()
        .fold(
          errors =>
            (
              journey,
              BadRequest(inspectionAddressPage(journey.getPotentialInspectionAddresses, errors, postAction))
            ).asFuture,
          {
            case Other     =>
              (journey, Redirect(startAddressLookup)).asFuture
            case Declarant =>
              journey.getDeclarantContactDetailsFromACC14
                .map { address =>
                  redirectToTheNextPage(modifyJourney(journey, InspectionAddress.ofType(Declarant).mapFrom(address)))
                }
                .getOrElse((journey, Redirect(baseRoutes.IneligibleController.ineligible())))
                .asFuture
            case Importer  =>
              journey.getConsigneeContactDetailsFromACC14
                .map { address =>
                  redirectToTheNextPage(modifyJourney(journey, InspectionAddress.ofType(Importer).mapFrom(address)))
                }
                .getOrElse((journey, Redirect(baseRoutes.IneligibleController.ineligible())))
                .asFuture
          }
        )
    },
    fastForwardToCYAEnabled = false
  )

}
