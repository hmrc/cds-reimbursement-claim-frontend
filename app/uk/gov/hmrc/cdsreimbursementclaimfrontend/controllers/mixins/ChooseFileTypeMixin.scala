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

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Request
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType

trait ChooseFileTypeMixin extends ClaimBaseController {

  implicit val errorHandler: ErrorHandler

  val uploadFilesRoute: Call

  def viewTemplate
    : (Form[Option[UploadDocumentType]], Seq[UploadDocumentType], Boolean) => Request[?] ?=> HtmlFormat.Appendable

  def modifyClaim(claim: Claim, documentType: UploadDocumentType): Either[String, Claim]

  final val show: Action[AnyContent] = actionReadClaim { claim =>
    claim.getDocumentTypesIfRequired match {
      case None =>
        Redirect(uploadFilesRoute)

      case Some(availableDocumentTypes) =>
        val form: Form[Option[UploadDocumentType]] =
          Forms.chooseFileTypeForm(availableDocumentTypes.toSet)

        Ok(
          viewTemplate(
            form,
            availableDocumentTypes,
            claim.answers.supportingEvidences.nonEmpty
          )
        )
    }
  }

  final val submit: Action[AnyContent] = actionReadWriteClaim(
    implicit request =>
      claim =>
        claim.getDocumentTypesIfRequired match {
          case None =>
            (claim, Redirect(uploadFilesRoute))

          case Some(availableDocumentTypes) =>
            val form: Form[Option[UploadDocumentType]] =
              Forms.chooseFileTypeForm(availableDocumentTypes.toSet)

            form
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  (
                    claim,
                    BadRequest(
                      viewTemplate(
                        formWithErrors,
                        availableDocumentTypes,
                        claim.answers.supportingEvidences.nonEmpty
                      )
                    )
                  ),
                {
                  case None =>
                    (claim, Redirect(checkYourAnswers))

                  case Some(documentType) =>
                    modifyClaim(claim, documentType)
                      .fold(
                        error => (claim, logAndDisplayError("Unexpected", error)),
                        upddatedClaim => (upddatedClaim, Redirect(uploadFilesRoute))
                      )
                }
              )
        },
    fastForwardToCYAEnabled = false
  )

}
