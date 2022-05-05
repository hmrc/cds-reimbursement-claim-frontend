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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.ChooseFileTypeController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => uploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{supportingevidence => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class ChooseFileTypeController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  sessionStore: SessionCache,
  chooseDocumentTypePage: pages.choose_document_type
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with SessionDataExtractor {

  val evidenceTypes: Seq[UploadDocumentType] = UploadDocumentType.c285EvidenceTypes

  def chooseSupportingEvidenceDocumentType(
    journey: JourneyBindable
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      Ok(
        chooseDocumentTypePage(
          journey,
          chooseSupportEvidenceDocumentTypeForm(evidenceTypes),
          getSupportingEvidenceHints(evidenceTypes),
          evidenceTypes,
          routes.ChooseFileTypeController.chooseSupportingEvidenceDocumentTypeSubmit(journey)
        )
      )
    }

  def chooseSupportingEvidenceDocumentTypeSubmit(
    journey: JourneyBindable
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        chooseSupportEvidenceDocumentTypeForm(evidenceTypes)
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                chooseDocumentTypePage(
                  journey,
                  requestFormWithErrors,
                  getSupportingEvidenceHints(evidenceTypes),
                  evidenceTypes,
                  routes.ChooseFileTypeController.chooseSupportingEvidenceDocumentTypeSubmit(journey)
                )
              ),
            documentType =>
              EitherT(
                updateSession(sessionStore, request)(
                  _.copy(
                    journeyStatus =
                      FillingOutClaim.from(fillingOutClaim)(_.copy(documentTypeAnswer = documentType.some)).some
                  )
                )
              )
                .fold(
                  logAndDisplayError("Error assigning evidence document type"),
                  _ => Redirect(uploadRoutes.SupportingEvidenceController.uploadSupportingEvidence(journey))
                )
          )
      }
    }

}

object ChooseFileTypeController {

  val chooseDocumentTypeDataKey: String = "supporting-evidence.choose-document-type"

  def chooseSupportEvidenceDocumentTypeForm(
    documentTypeList: Seq[UploadDocumentType]
  ): Form[UploadDocumentType] =
    Form(
      mapping(
        chooseDocumentTypeDataKey -> nonEmptyText
          .verifying(
            "error.invalid-document-type",
            key => UploadDocumentType.parse(key).exists(v => documentTypeList.contains(v))
          )
      )(UploadDocumentType.tryParse)(dt => Some(UploadDocumentType.keyOf(dt)))
    )

  def getSupportingEvidenceHints(documentTypeList: Seq[UploadDocumentType]): DropdownHints =
    DropdownHints.enumeration(documentTypeList)
}
