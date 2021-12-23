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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload

import cats.data.EitherT
import cats.data.NonEmptyList
import cats.implicits.catsSyntaxEq
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.FileUploadConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.SupportingEvidenceController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanFailure
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanSuccess
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{supportingevidence => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SupportingEvidenceController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  upscanService: UpscanService,
  sessionStore: SessionCache,
  config: FileUploadConfig,
  uploadPage: pages.upload,
  chooseDocumentTypePage: pages.choose_document_type,
  checkYourAnswersPage: pages.check_your_answers,
  scanProgressPage: pages.scan_progress,
  uploadFailedPage: pages.upload_failed,
  scanFailedPage: pages.scan_failed
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with SessionDataExtractor {

  lazy val maxUploads: Int = config.readMaxUploadsValue(supportingEvidenceKey)

  implicit val supportingEvidenceExtractor: DraftClaim => Option[SupportingEvidencesAnswer] =
    _.supportingEvidencesAnswer

  val evidenceTypes: Seq[UploadDocumentType] = UploadDocumentType.c285EvidenceTypes

  def uploadSupportingEvidence(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[SupportingEvidencesAnswer] { (_, answer, router) =>
        if (answer.exists(_.length >= maxUploads))
          Future.successful(Redirect(routes.SupportingEvidenceController.checkYourAnswers(journey)))
        else
          upscanService
            .initiate(
              routes.SupportingEvidenceController.handleUpscanErrorRedirect(journey),
              reference => routes.SupportingEvidenceController.scanProgress(journey, reference),
              config.readMaxFileSize(supportingEvidenceKey)
            )
            .fold(
              _ => errorHandler.errorResult(),
              upscanUpload => Ok(uploadPage(upscanUpload, getSupportingEvidenceHints(evidenceTypes), router.subKey))
            )
      }
    }

  def uploadSupportingEvidenceSubmit(journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async {
      Redirect(routes.SupportingEvidenceController.uploadSupportingEvidence(journey))
    }

  def handleUpscanErrorRedirect(journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData {
      Redirect(routes.SupportingEvidenceController.sizeFail(journey))
    }

  def sizeFail(journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(uploadFailedPage(journey))
    }

  def attachDocument(
    upload: UpscanUpload,
    callback: UpscanSuccess,
    answer: Option[SupportingEvidencesAnswer],
    claim: FillingOutClaim
  ): FillingOutClaim = {
    val newEvidence = UploadDocument(
      upload.uploadReference,
      upload.upscanUploadMeta,
      upload.uploadedOn,
      callback,
      callback.fileName,
      None
    )

    val evidences = answer.map(_ :+ newEvidence) orElse Some(SupportingEvidencesAnswer(newEvidence))

    FillingOutClaim.from(claim)(_.copy(supportingEvidencesAnswer = evidences))
  }

  def scanProgress(journey: JourneyBindable, uploadReference: UploadReference): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SupportingEvidencesAnswer] { (fillingOutReturn, maybeAnswers) =>
        val result = for {
          upscanUpload <- upscanService getUpscanUpload uploadReference
          _            <- upscanUpload.upscanCallBack match {
                            case Some(upscanSuccess: UpscanSuccess)
                                if maybeAnswers.forall(_.forall(_.uploadReference =!= uploadReference)) =>
                              EitherT(
                                updateSession(sessionStore, request)(
                                  _.copy(journeyStatus =
                                    attachDocument(upscanUpload, upscanSuccess, maybeAnswers, fillingOutReturn).some
                                  )
                                )
                              )
                            case _ => EitherT.pure[Future, Error](())
                          }
        } yield upscanUpload

        result.fold(
          _ => errorHandler.errorResult(),
          upscanUpload =>
            upscanUpload.upscanCallBack match {
              case Some(_: UpscanSuccess) =>
                Redirect(
                  routes.SupportingEvidenceController.chooseSupportingEvidenceDocumentType(journey, uploadReference)
                )
              case Some(_: UpscanFailure) =>
                Redirect(routes.SupportingEvidenceController.handleUpscanCallBackFailures(journey))
              case None                   =>
                Ok(scanProgressPage(journey, upscanUpload))
            }
        )
      }
    }

  def scanProgressSubmit(journey: JourneyBindable, uploadReference: String): Action[AnyContent] =
    authenticatedActionWithSessionData.async { _ =>
      Redirect(
        routes.SupportingEvidenceController.scanProgress(journey, UploadReference(uploadReference))
      )
    }

  def handleUpscanCallBackFailures(journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(scanFailedPage(journey))
    }

  def chooseSupportingEvidenceDocumentType(
    journey: JourneyBindable,
    uploadReference: UploadReference
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      Ok(
        chooseDocumentTypePage(
          journey,
          chooseSupportEvidenceDocumentTypeForm(evidenceTypes),
          getSupportingEvidenceHints(evidenceTypes),
          uploadReference,
          evidenceTypes
        )
      )
    }

  def chooseSupportingEvidenceDocumentTypeSubmit(
    journey: JourneyBindable,
    uploadReference: UploadReference
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SupportingEvidencesAnswer] { (fillingOutClaim, maybeEvidences) =>
        chooseSupportEvidenceDocumentTypeForm(evidenceTypes)
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                chooseDocumentTypePage(
                  journey,
                  requestFormWithErrors,
                  getSupportingEvidenceHints(evidenceTypes),
                  uploadReference,
                  evidenceTypes
                )
              ),
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
                          FillingOutClaim.from(fillingOutClaim)(_.copy(supportingEvidencesAnswer = evidences.some)).some
                      )
                    )
                  )
              } yield ()

              result.fold(
                logAndDisplayError("Error assigning evidence document type"),
                _ => Redirect(routes.SupportingEvidenceController.checkYourAnswers(journey))
              )
            }
          )
      }
    }

  def deleteSupportingEvidence(
    journey: JourneyBindable,
    uploadReference: UploadReference,
    addNew: Boolean
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SupportingEvidencesAnswer] { (fillingOutClaim, maybeEvidences) =>
        def removeEvidence(evidences: NonEmptyList[UploadDocument]) =
          NonEmptyList.fromList(evidences.filterNot(_.uploadReference === uploadReference))

        val newJourney =
          FillingOutClaim.from(fillingOutClaim)(
            _.copy(supportingEvidencesAnswer = maybeEvidences flatMap removeEvidence)
          )

        val result = for {
          _ <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newJourney))))
        } yield ()

        result.fold(
          logAndDisplayError("Could not update session"),
          _ =>
            Redirect(
              if (addNew) routes.SupportingEvidenceController.uploadSupportingEvidence(journey)
              else routes.SupportingEvidenceController.checkYourAnswers(journey)
            )
        )
      }
    }

  def checkYourAnswers(journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SupportingEvidencesAnswer] { (_, maybeSupportingEvidences) =>
        def redirectToUploadEvidence =
          Redirect(routes.SupportingEvidenceController.uploadSupportingEvidence(journey))

        def listUploadedItems(evidences: SupportingEvidencesAnswer) =
          Ok(checkYourAnswersPage(journey, evidences, maxUploads, whetherAddAnotherDocument))

        maybeSupportingEvidences.fold(redirectToUploadEvidence)(listUploadedItems)
      }
    }

  def checkYourAnswersSubmit(journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[SupportingEvidencesAnswer] { (_, maybeEvidences) =>
        maybeEvidences.fold(
          Redirect(routes.SupportingEvidenceController.uploadSupportingEvidence(journey))
        ) { evidences =>
          if (evidences.size >= maxUploads) {
            Redirect(claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journey))
          } else {
            whetherAddAnotherDocument
              .bindFromRequest()
              .fold(
                formWithErrors => BadRequest(checkYourAnswersPage(journey, evidences, maxUploads, formWithErrors)),
                {
                  case Yes =>
                    Redirect(routes.SupportingEvidenceController.uploadSupportingEvidence(journey))
                  case No  =>
                    Redirect(claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journey))
                }
              )
          }
        }
      }
    }
}

object SupportingEvidenceController {

  final case class ChooseSupportingEvidenceDocumentType(
    supportingEvidenceDocumentType: UploadDocumentType
  ) extends AnyVal

  val supportingEvidenceKey: String     = "supporting-evidence"
  val chooseDocumentTypeDataKey: String = "supporting-evidence.choose-document-type"
  val checkYourAnswersDataKey: String   = "supporting-evidence.check-your-answers"

  def chooseSupportEvidenceDocumentTypeForm(
    documentTypeList: Seq[UploadDocumentType]
  ): Form[ChooseSupportingEvidenceDocumentType] =
    Form(
      mapping(
        chooseDocumentTypeDataKey -> nonEmptyText
          .verifying(
            "supporting-evidence.error.invalid-document-type",
            key => UploadDocumentType.parse(key).map(v => documentTypeList.contains(v)).getOrElse(false)
          )
          .transform[UploadDocumentType](UploadDocumentType.tryParse, UploadDocumentType.keyOf)
      )(ChooseSupportingEvidenceDocumentType.apply)(ChooseSupportingEvidenceDocumentType.unapply)
    )

  val whetherAddAnotherDocument: Form[YesNo] =
    YesOrNoQuestionForm(checkYourAnswersDataKey)

  def getSupportingEvidenceHints(documentTypeList: Seq[UploadDocumentType]): DropdownHints =
    DropdownHints.enumeration(documentTypeList)
}
