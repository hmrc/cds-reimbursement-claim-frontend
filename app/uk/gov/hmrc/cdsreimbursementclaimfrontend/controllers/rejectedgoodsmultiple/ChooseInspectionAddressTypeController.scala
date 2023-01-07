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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.RejectedGoodsInspectionAddressLookupMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class ChooseInspectionAddressTypeController @Inject() (
  val jcc: JourneyControllerComponents,
  val addressLookupService: AddressLookupService,
  val inspectionAddressPage: pages.choose_inspection_address_type
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends RejectedGoodsMultipleJourneyBaseController
    with RejectedGoodsInspectionAddressLookupMixin {

  private val nextPage: Call =
    routes.CheckBankDetailsController.show()

  override val postAction: Call =
    routes.ChooseInspectionAddressTypeController.submit()

  override val problemWithAddressPage: Call =
    routes.ProblemWithAddressController.show()

  override val startAddressLookup: Call =
    routes.ChooseInspectionAddressTypeController.redirectToALF()

  override val retrieveLookupAddress: Call =
    routes.ChooseInspectionAddressTypeController.retrieveAddressFromALF()

  final override def modifyJourney(journey: Journey, inspectionAddress: InspectionAddress): Journey =
    journey.submitInspectionAddress(inspectionAddress)

  final override def modifyJourney(journey: Journey, contactAddress: ContactAddress): Journey =
    journey.submitInspectionAddress(InspectionAddress.ofType(Other).mapFrom(contactAddress))

  override def redirectToTheNextPage(journey: RejectedGoodsMultipleJourney): (RejectedGoodsMultipleJourney, Result) =
    (
      journey,
      if (journey.hasCompleteAnswers)
        Redirect(checkYourAnswers)
      else
        Redirect(nextPage)
    )
}
