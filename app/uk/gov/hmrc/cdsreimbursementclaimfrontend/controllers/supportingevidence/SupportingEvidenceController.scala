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
import play.api.Configuration
import play.api.data.Forms.{mapping, number}
import play.api.data._
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.upload.{FileUploadController, FileUploadServices}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceDocumentType.SupportingEvidenceDocumentTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, upscan => _, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{supportingevidence => pages}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SupportingEvidenceController @Inject() (
  authenticatedAction: AuthenticatedAction,
  sessionDataAction: SessionDataAction,
  upscanService: UpscanService,
  fileUploadServices: FileUploadServices,
  errorHandler: ErrorHandler,
  config: Configuration,
  uploadPage: pages.upload,
  chooseDocumentTypePage: pages.choose_document_type,
  checkYourAnswersPage: pages.check_your_answers,
  scanProgressPage: pages.scan_progress,
  uploadFailedPage: pages.upload_failed,
  scanFailedPage: pages.scan_failed
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, cc: MessagesControllerComponents, sessionStore: SessionCache)
    extends FileUploadController(
      authenticatedAction,
      sessionDataAction,
      upscanService,
      errorHandler,
      cc
    ) with SessionDataExtractor {

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

  //----

  lazy val maxUploads: Int =
    config.underlying.getInt(s"microservice.services.upscan-initiate.max-uploads")

  private def withUploadSupportingEvidenceAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      Option[SupportingEvidencesAnswer]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (s, r @ FillingOutClaim(_, _, c: DraftClaim)) =>
      f(s, r, c.fold(draftC285Claim = _.supportingEvidencesAnswer))
    })

  def handleUpscanCallBackFailures(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(scanFailedPage())
    }

  def scanProgress(
    uploadReference: UploadReference
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, fillingOutReturn, answers) =>
        val result = for {
          upscanUpload <- upscanService.getUpscanUpload(uploadReference)
          _            <- upscanUpload.upscanCallBack match {
                            case Some(upscanSuccess: UpscanSuccess)
                                if answers.forall(_.forall(_.uploadReference =!= uploadReference)) =>
                              storeUpscanSuccess(
                                upscanUpload,
                                upscanSuccess,
                                answers,
                                fillingOutReturn
                              )
                            case _ => EitherT.pure[Future, Error](())
                          }
        } yield upscanUpload

        result.fold(
          e => {
            logger.warn(s"could not update the status of upscan upload to uploaded : $e")
            errorHandler.errorResult()
          },
          upscanUpload =>
            upscanUpload.upscanCallBack match {
              case Some(_: UpscanSuccess) =>
                Redirect(routes.SupportingEvidenceController.chooseSupportingEvidenceDocumentType(uploadReference))
              case Some(_: UpscanFailure) =>
                Redirect(routes.SupportingEvidenceController.handleUpscanCallBackFailures())
              case None                   =>
                Ok(scanProgressPage(upscanUpload))
            }
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

  private def storeUpscanSuccess(
    upscanUpload: UpscanUpload,
    upscanCallBack: UpscanSuccess,
    supportingEvidencesAnswer: Option[SupportingEvidencesAnswer],
    fillingOutClaim: FillingOutClaim
  )(implicit
    request: RequestWithSessionData[_],
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] = {

    val newEvidence = SupportingEvidence(
      upscanUpload.uploadReference,
      upscanUpload.upscanUploadMeta,
      upscanUpload.uploadedOn,
      upscanCallBack,
      upscanCallBack.fileName,
      None
    )

    val evidences = supportingEvidencesAnswer.map(_ :+ newEvidence) orElse Some(SupportingEvidencesAnswer(newEvidence))

    val newJourney = FillingOutClaim.of(fillingOutClaim)(_.copy(supportingEvidencesAnswer = evidences))

    EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newJourney))))
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
      withUploadSupportingEvidenceAnswers { (_, fillingOutClaim, maybeEvidences: Option[SupportingEvidencesAnswer]) =>
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

  def deleteSupportingEvidence(
    uploadReference: UploadReference,
    addNew: Boolean
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, fillingOutClaim, maybeEvidences) =>
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
      withUploadSupportingEvidenceAnswers { (_, _, maybeSupportingEvidences) =>
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
