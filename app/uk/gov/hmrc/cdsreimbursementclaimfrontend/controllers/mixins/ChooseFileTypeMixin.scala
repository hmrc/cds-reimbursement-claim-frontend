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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

trait ChooseFileTypeMixin extends JourneyBaseController {

  implicit val errorHandler: ErrorHandler

  val uploadFilesRoute: Call

  def viewTemplate
    : (Form[Option[UploadDocumentType]], Seq[UploadDocumentType], Boolean) => Request[_] => HtmlFormat.Appendable

  def modifyJourney(journey: Journey, documentType: UploadDocumentType): Either[String, Journey]

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    (journey.getDocumentTypesIfRequired match {
      case None =>
        Redirect(uploadFilesRoute)

      case Some(availableDocumentTypes) =>
        val form: Form[Option[UploadDocumentType]] =
          Forms.chooseFileTypeForm(availableDocumentTypes.toSet)

        Ok(
          viewTemplate(
            form,
            availableDocumentTypes,
            journey.answers.supportingEvidences.nonEmpty
          )(request)
        )
    }).asFuture
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      (journey.getDocumentTypesIfRequired match {
        case None =>
          (journey, Redirect(uploadFilesRoute))

        case Some(availableDocumentTypes) =>
          val form: Form[Option[UploadDocumentType]] =
            Forms.chooseFileTypeForm(availableDocumentTypes.toSet)

          form
            .bindFromRequest()
            .fold(
              formWithErrors =>
                (
                  journey,
                  BadRequest(
                    viewTemplate(
                      formWithErrors,
                      availableDocumentTypes,
                      journey.answers.supportingEvidences.nonEmpty
                    )(request)
                  )
                ),
              {
                case None =>
                  (journey, Redirect(checkYourAnswers))

                case Some(documentType) =>
                  modifyJourney(journey, documentType)
                    .fold(
                      error => (journey, logAndDisplayError("Unexpected", error)),
                      upddatedJourney => (upddatedJourney, Redirect(uploadFilesRoute))
                    )
              }
            )
      }).asFuture
    },
    fastForwardToCYAEnabled = false
  )

}
