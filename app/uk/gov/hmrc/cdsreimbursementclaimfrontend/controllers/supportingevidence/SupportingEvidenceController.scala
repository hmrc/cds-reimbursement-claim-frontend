/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence

import cats.data.{EitherT, NonEmptyList}
import cats.implicits.{catsSyntaxEq, catsSyntaxOptionId}
import com.google.inject.{Inject, Singleton}
import play.api.{Configuration, Logging}
import play.api.data.Forms.{mapping, number}
import play.api.data._
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.upload.{FileUploadController, FileUploadServices}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceDocumentType.SupportingEvidenceDocumentTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{supportingevidence => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SupportingEvidenceController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val upscanService: UpscanService,
  val errorHandler: ErrorHandler,
  val sessionStore: SessionCache,
  fileUploadServices: FileUploadServices,
  config: Configuration,
  uploadPage: pages.upload,
  chooseDocumentTypePage: pages.choose_document_type,
  checkYourAnswersPage: pages.check_your_answers,
  scanProgressPage: pages.scan_progress,
  uploadFailedPage: pages.upload_failed,
  scanFailedPage: pages.scan_failed
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with FileUploadController
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with SessionDataExtractor {

  import fileUploadServices._

  implicit val supportingEvidenceExtractor: DraftC285Claim => Option[SupportingEvidencesAnswer] =
    _.supportingEvidencesAnswer

  def uploadSupportingEvidence(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SupportingEvidencesAnswer] { (_, answer) =>
        initiateUpload(answer)(upscanUpload => Ok(uploadPage(upscanUpload)))
      }
    }

  def handleUpscanErrorRedirect(): Action[AnyContent] =
    authenticatedActionWithSessionData {
      Redirect(routes.SupportingEvidenceController.documentDidNotUpload())
    }

  def documentDidNotUpload(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(uploadFailedPage())
    }

  def scanProgress(uploadReference: UploadReference): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SupportingEvidencesAnswer] { (fillingOutReturn, answer) =>
        handleUploadCallback(uploadReference, fillingOutReturn, answer)(uploadReference =>
          Ok(scanProgressPage(uploadReference))
        )
      }
    }

  def scanProgressSubmit(
    uploadReference: String
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { _ =>
      Redirect(
        routes.SupportingEvidenceController.scanProgress(UploadReference(uploadReference))
      )
    }

  def handleUpscanCallBackFailures(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(scanFailedPage())
    }

  def chooseSupportingEvidenceDocumentType(uploadReference: UploadReference): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      Ok(
        chooseDocumentTypePage(
          SupportingEvidenceController.chooseSupportEvidenceDocumentTypeForm,
          uploadReference
        )
      )
    }

  def chooseSupportingEvidenceDocumentTypeSubmit(uploadReference: UploadReference): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SupportingEvidencesAnswer] { (fillingOutClaim, maybeEvidences) =>
        SupportingEvidenceController.chooseSupportEvidenceDocumentTypeForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors => BadRequest(chooseDocumentTypePage(requestFormWithErrors, uploadReference)),
            documentType => {
              val answers = for {
                documents <- maybeEvidences.map(_.toList)
                index     <- Option(documents.indexWhere(_.uploadReference === uploadReference)).filter(_ >= 0)
                (x, xs)    = documents.splitAt(index)
                updated    = documents(index).copy(documentType = Some(documentType.supportingEvidenceDocumentType))
                items     <- NonEmptyList.fromList(updated :: (x ++ xs.drop(1)))
              } yield items

              val result = for {
                evidences <-
                  EitherT
                    .fromOption[Future](answers, Error(s"could not find file upload with reference: $uploadReference"))
                _         <-
                  EitherT(
                    updateSession(sessionStore, request)(
                      _.copy(
                        journeyStatus =
                          FillingOutClaim.of(fillingOutClaim)(_.copy(supportingEvidencesAnswer = evidences.some)).some
                      )
                    )
                  )
              } yield ()

              result.fold(
                e => {
                  logger.warn("Error assigning evidence document type", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.SupportingEvidenceController.checkYourAnswers())
              )
            }
          )
      }
    }

  //----

  lazy val maxUploads: Int =
    config.underlying.getInt(s"microservice.services.upscan-initiate.supporting-evidence.max-uploads")

  def deleteSupportingEvidence(
    uploadReference: UploadReference,
    addNew: Boolean
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SupportingEvidencesAnswer] { (fillingOutClaim, maybeEvidences) =>
        def removeEvidence(evidences: NonEmptyList[SupportingEvidence]) =
          NonEmptyList.fromList(evidences.filterNot(_.uploadReference === uploadReference))

        val newDraftClaim = fillingOutClaim.draftClaim.fold(
          _.copy(supportingEvidencesAnswer = maybeEvidences flatMap removeEvidence)
        )

        val newJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

        val result = for {
          _ <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newJourney))))
        } yield ()

        result.fold(
          e => {
            logger.warn("Could not update session", e)
            errorHandler.errorResult()
          },
          _ =>
            Redirect(
              if (addNew) routes.SupportingEvidenceController.uploadSupportingEvidence()
              else routes.SupportingEvidenceController.checkYourAnswers()
            )
        )
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SupportingEvidencesAnswer] { (_, maybeSupportingEvidences) =>
        def redirectToUploadEvidence =
          Redirect(routes.SupportingEvidenceController.uploadSupportingEvidence())

        def listUploadedItems(evidences: SupportingEvidencesAnswer) =
          Ok(checkYourAnswersPage(evidences, maxUploads))

        maybeSupportingEvidences.fold(redirectToUploadEvidence)(listUploadedItems)
      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async {
      Redirect(claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers())
    }
}

object SupportingEvidenceController {

  final case class ChooseSupportingEvidenceDocumentType(
    supportingEvidenceDocumentType: SupportingEvidenceDocumentType
  )

  val chooseDocumentTypeDataKey: String = "supporting-evidence.choose-document-type"

  val chooseSupportEvidenceDocumentTypeForm: Form[ChooseSupportingEvidenceDocumentType] =
    Form(
      mapping(
        chooseDocumentTypeDataKey -> number
          .verifying(
            "invalid supporting evidence document type",
            documentTypeIndex => SupportingEvidenceDocumentTypes.indices.contains(documentTypeIndex)
          )
          .transform[SupportingEvidenceDocumentType](
            documentTypeIndex => SupportingEvidenceDocumentTypes(documentTypeIndex),
            documentType => SupportingEvidenceDocumentTypes.indexOf(documentType)
          )
      )(ChooseSupportingEvidenceDocumentType.apply)(ChooseSupportingEvidenceDocumentType.unapply)
    )
}
