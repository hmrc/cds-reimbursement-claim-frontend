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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import com.google.inject.Singleton
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.choose_document_type
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms

@Singleton
class ChooseFileTypeController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  sessionStore: SessionCache,
  chooseDocumentTypePage: choose_document_type
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with SessionDataExtractor {

  val evidenceTypes: Seq[UploadDocumentType] = UploadDocumentType.c285EvidenceTypes

  val show: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      Ok(
        chooseDocumentTypePage(
          JourneyBindable.Multiple,
          Forms.chooseSupportEvidenceDocumentTypeForm(evidenceTypes),
          DropdownHints.enumeration(evidenceTypes),
          evidenceTypes,
          routes.ChooseFileTypeController.submit
        )
      )
    }

  val submit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        Forms
          .chooseSupportEvidenceDocumentTypeForm(evidenceTypes)
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                chooseDocumentTypePage(
                  JourneyBindable.Multiple,
                  requestFormWithErrors,
                  DropdownHints.enumeration(evidenceTypes),
                  evidenceTypes,
                  routes.ChooseFileTypeController.submit
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
                  _ =>
                    Redirect(
                      uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes.UploadFilesController
                        .show(JourneyBindable.Multiple)
                    )
                )
          )
      }
    }

}
