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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.EoriDetailsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.eoriNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_new_eori_number

trait EnterNewEoriNumberMixin extends ClaimBaseController {

  type Claim <: claims.Claim & claims.ClaimBase & claims.OverpaymentsClaimProperties

  val eoriDetailsConnector: EoriDetailsConnector
  val newEoriPage: enter_new_eori_number
  val postAction: Call
  val continueAction: Call
  val formKey: String = "enter-new-eori-number"

  def modifyClaim(claim: Claim, eori: Eori): Claim

  def getNewEoriAnswer(claim: Claim): Option[Eori] =
    claim.answers.newEori

  def getImporterEori(claim: Claim) =
    claim.getConsigneeEoriFromACC14.getOrElse(claim.answers.userEoriNumber)

  private def newEoriStartWithValidCountryCode(newEori: Eori, importerEori: Eori): Option[String] = importerEori match
    case importEori if importEori.isGBEori && !newEori.isGBEori                        => Some("mustStartWithGB")
    case importEori if (importEori.isXiEori | importEori.isEuEori) && newEori.isGBEori => Some("mustNotStartWithGB")
    case _                                                                             => None

  final val show: Action[AnyContent] = actionReadClaim { claim =>
    Ok(
      newEoriPage(
        eoriNumberForm(formKey)
          .withDefault(getNewEoriAnswer(claim)),
        postAction
      )
    )

  }

  final val submit: Action[AnyContent] =
    actionReadWriteClaim { claim =>
      eoriNumberForm(formKey)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              BadRequest(
                newEoriPage(
                  formWithErrors,
                  postAction
                )
              )
            ),
          eori =>
            newEoriStartWithValidCountryCode(eori, getImporterEori(claim)) match {
              case Some(errorMessageKey) =>
                (
                  claim,
                  BadRequest(
                    newEoriPage(
                      eoriNumberForm(formKey)
                        .fill(eori)
                        .withError(FormError("enter-new-eori-number", errorMessageKey)),
                      postAction
                    )
                  )
                )
              case None                  =>
                eoriDetailsConnector.getEoriDetails(eori).map {
                  case Some(_) =>
                    (modifyClaim(claim, eori), Redirect(continueAction))

                  case None =>
                    (
                      claim,
                      BadRequest(
                        newEoriPage(
                          eoriNumberForm(formKey)
                            .fill(eori)
                            .withError(FormError("enter-new-eori-number", "doesNotExist")),
                          postAction
                        )
                      )
                    )
                }
            }
        )
    }
}
