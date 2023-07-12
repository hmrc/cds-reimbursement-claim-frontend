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
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.RejectedGoodsInspectionAddressLookupMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType.Other
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import scala.concurrent.ExecutionContext

@Singleton
class ChooseInspectionAddressTypeController @Inject() (
  val jcc: JourneyControllerComponents,
  val addressLookupService: AddressLookupService,
  val inspectionAddressPage: pages.choose_inspection_address_type
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext, val errorHandler: ErrorHandler)
    extends RejectedGoodsSingleJourneyBaseController
    with RejectedGoodsInspectionAddressLookupMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  override val postAction: Call =
    routes.ChooseInspectionAddressTypeController.submit()

  override val problemWithAddressPage: Call = routes.ProblemWithAddressController.show()

  override val startAddressLookup: Call =
    routes.ChooseInspectionAddressTypeController.redirectToALF()

  override val retrieveLookupAddress: Call =
    routes.ChooseInspectionAddressTypeController.retrieveAddressFromALF()

  final override def modifyJourney(journey: Journey, inspectionAddress: InspectionAddress): Journey =
    journey.submitInspectionAddress(inspectionAddress)

  final override def modifyJourney(journey: Journey, contactAddress: ContactAddress): Journey =
    journey.submitInspectionAddress(InspectionAddress.ofType(Other).mapFrom(contactAddress))

  override def redirectToTheNextPage(journey: RejectedGoodsSingleJourney): (RejectedGoodsSingleJourney, Result) =
    if (journey.hasCompleteAnswers)
      (journey, Redirect(checkYourAnswers))
    else if (journey.isAllSelectedDutiesAreCMAEligible)
      (journey, Redirect(routes.ChooseRepaymentMethodController.show()))
    else
      (
        journey,
        Redirect(
          if (journey.needsBanksAccountDetailsSubmission)
            routes.CheckBankDetailsController.show()
          else
            routes.UploadFilesController.show()
        )
      )
}
