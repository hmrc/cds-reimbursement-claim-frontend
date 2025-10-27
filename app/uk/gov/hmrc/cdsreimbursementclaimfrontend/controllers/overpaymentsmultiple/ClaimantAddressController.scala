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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.mvc.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.AddressLookupMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim.Checks.hasMRNAndImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class ClaimantAddressController @Inject() (
  val jcc: ClaimControllerComponents,
  val addressLookupService: AddressLookupService
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends OverpaymentsMultipleClaimBaseController
    with AddressLookupMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsMultipleClaim]] =
    Some(hasMRNAndImportDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val startAddressLookup: Call =
    routes.ClaimantAddressController.redirectToALF

  override val problemWithAddressPage: Call = routes.ProblemWithAddressController.show

  override val retrieveLookupAddress: Call =
    routes.ClaimantAddressController.retrieveAddressFromALF()

  override def modifyClaim(claim: Claim, contactAddress: ContactAddress): Claim =
    claim.submitContactAddress(contactAddress)

  override def redirectToTheNextPage(claim: OverpaymentsMultipleClaim): (OverpaymentsMultipleClaim, Result) =
    (claim, Redirect(routes.CheckYourAnswersController.show))
}
