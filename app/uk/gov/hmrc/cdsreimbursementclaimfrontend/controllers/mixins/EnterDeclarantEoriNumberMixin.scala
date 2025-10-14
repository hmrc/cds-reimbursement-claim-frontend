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

import play.api.data.FormError
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.eoriNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimValidationErrors
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_declarant_eori_number

import scala.concurrent.Future

trait EnterDeclarantEoriNumberMixin extends ClaimBaseController {

  val postAction: Call
  val continueAction: Call
  val whenEoriInputNotRequiredAction: Call
  val enterDeclarantEoriNumber: enter_declarant_eori_number

  def modifyClaim(claim: Claim, eori: Eori): Either[String, Claim]

  def needsEoriSubmission(claim: Claim): Boolean =
    claim.needsDeclarantAndConsigneeEoriSubmission

  def getEoriNumberAnswer(claim: Claim): Option[Eori] =
    claim.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber)

  val eoriNumberFormKey: String = "enter-declarant-eori-number"

  final val show: Action[AnyContent] =
    actionReadClaim { claim =>
      Future.successful {
        if !needsEoriSubmission(claim) then Redirect(whenEoriInputNotRequiredAction)
        else {
          val form = eoriNumberForm(eoriNumberFormKey).withDefault(getEoriNumberAnswer(claim))
          Ok(
            enterDeclarantEoriNumber(
              form,
              postAction
            )
          )
        }
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim { claim =>
      if !needsEoriSubmission(claim) then (claim, Redirect(whenEoriInputNotRequiredAction))
      else {
        eoriNumberForm(eoriNumberFormKey)
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Future.successful(
                (
                  claim,
                  BadRequest(
                    enterDeclarantEoriNumber(
                      formWithErrors,
                      postAction
                    )
                  )
                )
              ),
            eori =>
              Future.successful(
                modifyClaim(claim, eori)
                  .fold(
                    {
                      case ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI |
                          ClaimValidationErrors.SHOULD_MATCH_ACC14_DUPLICATE_DECLARANT_EORI =>
                        (
                          claim,
                          BadRequest(
                            enterDeclarantEoriNumber(
                              eoriNumberForm(eoriNumberFormKey)
                                .withError(
                                  FormError(
                                    eoriNumberFormKey,
                                    "eori-should-match-declarant"
                                  )
                                )
                                .withDefault(Some(eori)),
                              postAction
                            )
                          )
                        )
                      case errors =>
                        (claim, Redirect(baseRoutes.IneligibleController.ineligible))
                    },
                    updatedClaim => (updatedClaim, Redirect(continueAction))
                  )
              )
          )
      }
    }
}
