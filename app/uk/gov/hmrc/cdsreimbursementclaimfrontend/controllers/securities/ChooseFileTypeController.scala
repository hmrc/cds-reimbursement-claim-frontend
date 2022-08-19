/*
 * Copyright 2022 HM Revenue & Customs
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

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.choose_file_type

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler

@Singleton
class ChooseFileTypeController @Inject() (
  val jcc: JourneyControllerComponents,
  chooseFileTypePage: choose_file_type
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends SecuritiesJourneyBaseController {

  val submitAction: Call          = routes.ChooseFileTypeController.submit()
  val chooseFilesPageAction: Call = routes.UploadFilesController.show()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    (journey.getDocumentTypesIfRequired match {
      case None =>
        Redirect(routes.UploadFilesController.show())

      case Some(availableDocumentTypes) =>
        val form: Form[Option[UploadDocumentType]] =
          Forms.chooseFileTypeForm(availableDocumentTypes.toSet)

        Ok(
          chooseFileTypePage(
            form.withDefault(Some(journey.answers.selectedDocumentType)),
            availableDocumentTypes,
            submitAction
          )
        )
    }).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      (journey.getDocumentTypesIfRequired match {
        case None =>
          (journey, Redirect(routes.UploadFilesController.show()))

        case Some(availableDocumentTypes) =>
          val form: Form[Option[UploadDocumentType]] =
            Forms.chooseFileTypeForm(availableDocumentTypes.toSet)

          form.bindFromRequest
            .fold(
              formWithErrors =>
                (
                  journey,
                  BadRequest(
                    chooseFileTypePage(
                      formWithErrors,
                      availableDocumentTypes,
                      submitAction
                    )
                  )
                ),
              {
                case None =>
                  (journey, Redirect(checkYourAnswers))

                case Some(documentType) =>
                  journey
                    .submitDocumentTypeSelection(documentType)
                    .fold(
                      error => (journey, logAndDisplayError("Unexpected", error)),
                      upddatedJourney => (upddatedJourney, Redirect(chooseFilesPageAction))
                    )
              }
            )
      }).asFuture
    },
    fastForwardToCYAEnabled = false
  )

}
