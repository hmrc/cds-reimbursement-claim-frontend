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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim.Checks.hasMRNAndImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.enter_additional_details

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterAdditionalDetailsController @Inject() (
  val jcc: ClaimControllerComponents,
  enterAdditionalDetailsPage: enter_additional_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesClaimBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesClaim]] =
    Some(hasMRNAndImportDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val postAction: Call = routes.EnterAdditionalDetailsController.submit

  final def continueRoute(claim: Claim): Call =
    if claim.answers.contactDetails.isDefined
      && claim.answers.contactAddress.isDefined
    then routes.CheckYourAnswersController.show
    else routes.EnterContactDetailsController.show

  final val show: Action[AnyContent] =
    actionReadWriteClaim { claim =>
      val form: Form[String] =
        Forms.enterAdditionalDetailsSecuritiesForm.withDefault(claim.answers.additionalDetails)

      (
        claim.submitAdditionalDetailsPageVisited(true),
        Ok(
          enterAdditionalDetailsPage(
            form,
            postAction
          )
        )
      )
    }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim { claim =>
      Forms.enterAdditionalDetailsSecuritiesForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              BadRequest(
                enterAdditionalDetailsPage(
                  formWithErrors,
                  postAction
                )
              )
            ),
          additionalDetails =>
            (
              claim.submitAdditionalDetails(additionalDetails),
              Redirect(continueRoute(claim))
            )
        )
    }

}
