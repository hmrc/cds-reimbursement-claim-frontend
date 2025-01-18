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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.*
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.ContactAddressLookupMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_claimant_details

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimantDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val addressLookupService: AddressLookupService,
  claimantDetailsPage: check_claimant_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends SecuritiesJourneyBaseController
    with ContactAddressLookupMixin {

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final override def modifyJourney(journey: Journey, enterContactDetailsMode: Boolean): Journey =
    journey.withEnterContactDetailsMode(enterContactDetailsMode)

  override val problemWithAddressPage: Call = routes.ProblemWithAddressController.show

  val postAction: Call = routes.CheckClaimantDetailsController.submit
  val changeCd: Call   = routes.EnterContactDetailsController.show

  override val retrieveLookupAddress: Call =
    routes.CheckClaimantDetailsController.retrieveAddressFromALF()

  val startAddressLookup: Call = routes.CheckClaimantDetailsController.redirectToALF()

  override def viewTemplate: MrnContactDetails => ContactAddress => Request[?] => HtmlFormat.Appendable =
    cd => ca => implicit request => claimantDetailsPage(cd, ca, changeCd, Some(startAddressLookup), postAction)

  override val redirectWhenNoAddressDetailsFound: Call =
    routes.EnterMovementReferenceNumberController.show

  override val confirmEmailRoute: Call =
    routes.EnterContactDetailsController.show

  override val nextPageInTheJourney: Call =
    routes.ConfirmFullRepaymentController.showFirst

  override def modifyJourney(journey: Journey, contactDetails: MrnContactDetails): Journey =
    journey.submitContactDetails(Some(contactDetails))

  override def modifyJourney(journey: Journey, contactAddress: ContactAddress): Journey =
    journey.submitContactAddress(contactAddress)

  override def redirectToTheNextPage(journey: SecuritiesJourney): (SecuritiesJourney, Result) =
    if journey.userHasSeenCYAPage then {
      (journey, Redirect(routes.CheckYourAnswersController.show))
    } else {
      (journey, Redirect(routes.CheckClaimantDetailsController.show))
    }
}

object CheckClaimantDetailsController {
  val checkContactDetailsKey: String = "check-claimant-details"
}
