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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.mvc.*
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.ContactAddressLookupMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim.Checks.hasMRNAndImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_claimant_details

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimantDetailsController @Inject() (
  val jcc: ClaimControllerComponents,
  val addressLookupService: AddressLookupService,
  claimantDetailsPage: check_claimant_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends OverpaymentsScheduledClaimBaseController
    with ContactAddressLookupMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsScheduledClaim]] =
    Some(hasMRNAndImportDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val startAddressLookup: Call =
    routes.CheckClaimantDetailsController.redirectToALF

  val changeCd: Call =
    routes.EnterContactDetailsController.show

  val postAction: Call =
    routes.CheckClaimantDetailsController.submit

  override def viewTemplate: MrnContactDetails => ContactAddress => Request[?] ?=> HtmlFormat.Appendable =
    cd => ca => claimantDetailsPage(cd, ca, changeCd, Some(startAddressLookup), postAction)

  override val redirectWhenNoAddressDetailsFound: Call =
    routes.EnterMovementReferenceNumberController.show

  override val confirmEmailRoute: Call =
    routes.EnterContactDetailsController.show

  override val nextPageInTheClaim: Call =
    routes.CheckYourAnswersController.show

  override val problemWithAddressPage: Call = routes.ProblemWithAddressController.show

  override val retrieveLookupAddress: Call =
    routes.CheckClaimantDetailsController.retrieveAddressFromALF()

  override def modifyClaim(claim: Claim, contactAddress: ContactAddress): Claim =
    claim.submitContactAddress(contactAddress)

  override def redirectToTheNextPage(claim: OverpaymentsScheduledClaim): (OverpaymentsScheduledClaim, Result) =
    if claim.userHasSeenCYAPage then {
      (claim, Redirect(routes.CheckYourAnswersController.show))
    } else {
      (claim, Redirect(routes.CheckClaimantDetailsController.show))
    }
}
