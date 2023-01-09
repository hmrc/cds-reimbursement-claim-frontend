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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2

import com.github.arturopala.validator.Validator.Validate
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.basisOfOverpaymentClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks.hasMRNAndDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_basis_for_claim

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints

@Singleton
class BasisForClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  basisForClaimPage: select_basis_for_claim
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsSingleJourneyBaseController {

  val formKey: String = "select-basis-for-claim"

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  private val basisOfClaimsHints: DropdownHints =
    DropdownHints.range(elementIndex = 0, maxHints = 14)

  final val show: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      Future.successful {
        val form: Form[BasisOfOverpaymentClaim] =
          basisOfOverpaymentClaimForm.withDefault(journey.answers.basisOfClaim)
        Ok(
          basisForClaimPage(
            form,
            journey.getAvailableClaimTypes,
            basisOfClaimsHints,
            None,
            routes.BasisForClaimController.submit
          )
        )
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      basisOfOverpaymentClaimForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(
                  basisForClaimPage(
                    formWithErrors,
                    journey.getAvailableClaimTypes,
                    basisOfClaimsHints,
                    None,
                    routes.BasisForClaimController.submit
                  )
                )
              )
            ),
          basisOfClaim =>
            Future.successful(
              (
                journey.submitBasisOfClaim(basisOfClaim),
                Redirect(basisOfClaim match {
                  case BasisOfOverpaymentClaim.DuplicateEntry =>
                    routes.EnterDuplicateMovementReferenceNumberController.show
                  case _                                      =>
                    routes.EnterAdditionalDetailsController.show
                })
              )
            )
        )
    }
}
