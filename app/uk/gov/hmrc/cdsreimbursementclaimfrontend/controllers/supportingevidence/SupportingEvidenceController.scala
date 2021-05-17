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

import cats.data.EitherT
import cats.implicits.catsSyntaxEq
import com.google.inject.{Inject, Singleton}
import configs.ConfigReader
import configs.syntax._
import play.api.Configuration
import play.api.data.Forms.{mapping, number}
import play.api.data._
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceAnswer.{CompleteSupportingEvidenceAnswer, IncompleteSupportingEvidenceAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceDocumentType.SupportingEvidenceDocumentTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, upscan => _, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{supportingevidence => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SupportingEvidenceController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  upscanService: UpscanService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  chooseDocumentTypePage: pages.choose_document_type,
  uploadPage: pages.upload,
  checkYourAnswersPage: pages.check_your_answers,
  scanProgressPage: pages.scan_progress,
  uploadFailedPage: pages.upload_failed,
  scanFailedPage: pages.scan_failed
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def getUpscanInitiateConfig[A : ConfigReader](key: String): A =
    config.underlying
      .get[A](s"microservice.services.upscan-initiate.$key")
      .value

  private val maxUploads: Int = getUpscanInitiateConfig[Int]("max-uploads")

  private def withUploadSupportingEvidenceAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      SupportingEvidenceAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              s,
              r @ FillingOutClaim(_, _, c: DraftClaim)
            )
          ) =>
        val maybeSupportingEvidenceAnswers = c.fold(
          _.supportingEvidenceAnswers
        )
        maybeSupportingEvidenceAnswers.fold[Future[Result]](
          f(s, r, IncompleteSupportingEvidenceAnswer.empty)
        )(f(s, r, _))

      case _ => Redirect(baseRoutes.StartController.start())
    }

  def uploadSupportingEvidence(isAmend: Boolean): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, fillingOutClaim, answers) =>
        if (answers.fold(_.evidences, _.evidences).length >= maxUploads)
          Redirect(routes.SupportingEvidenceController.checkYourAnswers(isAmend))
        else {
          def purgeSession() = {
            val incomplete = answers.fold(
              incomplete => incomplete,
              complete => IncompleteSupportingEvidenceAnswer(complete.evidences)
            )

            val newDraftClaim = fillingOutClaim.draftClaim.fold(
              _.copy(supportingEvidenceAnswers = Some(incomplete))
            )

            val newJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

            EitherT(
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(newJourney))
              )
            )
          }

          val uploadUpscan = for {
            _      <- if (isAmend) purgeSession() else EitherT.rightT[Future, Error](())
            result <- upscanService
                        .initiate(
                          routes.SupportingEvidenceController
                            .handleUpscanErrorRedirect(),
                          routes.SupportingEvidenceController.scanProgress
                        )
          } yield result

          uploadUpscan
            .fold(
              e => {
                logger.warn("could not start upload supporting evidence", e)
                errorHandler.errorResult()
              },
              uploadUpscan =>
                Ok(
                  uploadPage(
                    uploadUpscan,
                    claimRoutes.BankAccountController.checkBankAccountDetails()
                  )
                )
            )
        }
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

  def handleUpscanCallBackFailures(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      Ok(scanFailedPage())
    }

  def scanProgress(
    uploadReference: UploadReference
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, fillingOutReturn, answers) =>
        answers match {
          case _: CompleteSupportingEvidenceAnswer =>
            Redirect(routes.SupportingEvidenceController.checkYourAnswers())

          case incompleteAnswers: IncompleteSupportingEvidenceAnswer =>
            val result = for {
              upscanUpload <- upscanService.getUpscanUpload(uploadReference)
              _            <- upscanUpload.upscanCallBack match {
                                case Some(upscanSuccess: UpscanSuccess) =>
                                  storeUpscanSuccess(
                                    upscanUpload,
                                    upscanSuccess,
                                    incompleteAnswers,
                                    fillingOutReturn
                                  )
                                case _                                  =>
                                  EitherT.pure[Future, Error](())
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
    }

  def scanProgressSubmit(
    uploadReference: String
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { _ =>
      Redirect(
        routes.SupportingEvidenceController
          .scanProgress(UploadReference(uploadReference))
      )
    }

  private def storeUpscanSuccess(
    upscanUpload: UpscanUpload,
    upscanCallBack: UpscanSuccess,
    answers: IncompleteSupportingEvidenceAnswer,
    fillingOutClaim: FillingOutClaim
  )(implicit
    request: RequestWithSessionData[_],
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] = {
    val newAnswers =
      upscanCallBack match {
        case success: UpscanSuccess =>
          val supportingEvidence =
            SupportingEvidence(
              upscanUpload.uploadReference,
              upscanUpload.upscanUploadMeta,
              upscanUpload.uploadedOn,
              success,
              success.fileName,
              None
            )
          answers.copy(evidences = supportingEvidence :: answers.evidences)

      }

    val newDraftClaim = fillingOutClaim.draftClaim.fold(
      _.copy(supportingEvidenceAnswers = Some(newAnswers))
    )
    val newJourney    = fillingOutClaim.copy(draftClaim = newDraftClaim)

    EitherT(
      updateSession(sessionStore, request)(
        _.copy(journeyStatus = Some(newJourney))
      )
    )
  }

  def chooseSupportingEvidenceDocumentType(uploadReference: UploadReference): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, _, answers) =>
        answers match {
          case _: IncompleteSupportingEvidenceAnswer | _: CompleteSupportingEvidenceAnswer =>
            Ok(
              chooseDocumentTypePage(
                SupportingEvidenceController.chooseSupportEvidenceDocumentTypeForm,
                uploadReference
              )
            )
        }
      }
    }

  def chooseSupportingEvidenceDocumentTypeSubmit(uploadReference: UploadReference): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, fillingOutClaim, answers) =>
        SupportingEvidenceController.chooseSupportEvidenceDocumentTypeForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors => BadRequest(chooseDocumentTypePage(requestFormWithErrors, uploadReference)),
            documentType => {
              val updatedAnswers = answers.fold(
                incomplete => {
                  val evidences: List[SupportingEvidence] =
                    incomplete.evidences.filter(p => p.uploadReference === uploadReference)
                  val s: SupportingEvidence               = evidences.headOption match {
                    case Some(fileUpload) =>
                      fileUpload.copy(documentType = Some(documentType.supportingEvidenceDocumentType))
                    case None             =>
                      sys.error("could not find uploaded file")
                  }
                  incomplete.copy(
                    evidences = incomplete.evidences
                      .filterNot(_.uploadReference === uploadReference) :+ s
                  )
                },
                { complete =>
                  val fu: List[SupportingEvidence] =
                    complete.evidences.filter(p => p.uploadReference === uploadReference)
                  val s: SupportingEvidence        = fu.headOption match {
                    case Some(fileUpload) =>
                      fileUpload.copy(documentType = Some(documentType.supportingEvidenceDocumentType))
                    case None             =>
                      sys.error(s"could not find file upload with reference: $uploadReference")
                  }
                  val newEvidences                 = complete.evidences.filterNot(_.uploadReference === uploadReference)
                  IncompleteSupportingEvidenceAnswer(
                    newEvidences :+ s
                  )
                }
              )
              val newDraftClaim  = fillingOutClaim.draftClaim.fold(
                _.copy(supportingEvidenceAnswers = Some(updatedAnswers))
              )
              val newJourney     = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = for {
                _ <- EitherT(
                       updateSession(sessionStore, request)(
                         _.copy(journeyStatus = Some(newJourney))
                       )
                     )
              } yield ()

              result.fold(
                e => {
                  logger.warn("Could not update session", e)
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
    isAmend: Boolean,
    addNew: Boolean
  ): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, fillingOutClaim, answers) =>
        val updatedAnswers = answers.fold(
          incomplete =>
            incomplete.copy(
              evidences = incomplete.evidences
                .filterNot(_.uploadReference === uploadReference)
            ),
          { complete =>
            val newEvidences = complete.evidences
              .filterNot(_.uploadReference === uploadReference)
            IncompleteSupportingEvidenceAnswer(
              newEvidences
            )
          }
        )

        val newDraftClaim = fillingOutClaim.draftClaim.fold(
          _.copy(supportingEvidenceAnswers = Some(updatedAnswers))
        )
        val newJourney    = fillingOutClaim.copy(draftClaim = newDraftClaim)

        val result = for {
          _ <- EitherT(
                 updateSession(sessionStore, request)(
                   _.copy(journeyStatus = Some(newJourney))
                 )
               )
        } yield ()

        result.fold(
          e => {
            logger.warn("Could not update session", e)
            errorHandler.errorResult()
          },
          _ =>
            if (addNew)
              Redirect(
                routes.SupportingEvidenceController.uploadSupportingEvidence()
              )
            else
              Redirect(routes.SupportingEvidenceController.checkYourAnswers(isAmend))
        )
      }
    }

  def checkYourAnswers(isAmend: Boolean): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, _, answers) =>
        checkYourAnswersHandler(answers, isAmend)
      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, fillingOutClaim, answers) =>
        val updatedAnswers: SupportingEvidenceAnswer = answers match {
          case IncompleteSupportingEvidenceAnswer(
                evidences
              ) =>
            CompleteSupportingEvidenceAnswer(
              evidences
            )
          case CompleteSupportingEvidenceAnswer(
                evidences
              ) =>
            CompleteSupportingEvidenceAnswer(
              evidences
            )
        }

        val newDraftClaim: DraftClaim.DraftC285Claim = fillingOutClaim.draftClaim.fold(
          _.copy(supportingEvidenceAnswers = Some(updatedAnswers))
        )
        val newJourney                               = fillingOutClaim.copy(draftClaim = newDraftClaim)

        val result = for {
          _ <- EitherT(
                 updateSession(sessionStore, request)(
                   _.copy(journeyStatus = Some(newJourney))
                 )
               )
        } yield ()

        result.fold(
          e => {
            logger.warn("Could not update session", e)
            errorHandler.errorResult()
          },
          _ => Redirect(claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers())
        )
      }
    }

  private def checkYourAnswersHandler(
    answers: SupportingEvidenceAnswer,
    isAmend: Boolean
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {

      case IncompleteSupportingEvidenceAnswer(
            supportingEvidences
          ) if supportingEvidences.isEmpty =>
        Redirect(routes.SupportingEvidenceController.uploadSupportingEvidence())

      case IncompleteSupportingEvidenceAnswer(
            supportingEvidences
          ) =>
        Ok(
          checkYourAnswersPage(
            CompleteSupportingEvidenceAnswer(supportingEvidences),
            maxUploads,
            isAmend
          )
        )

      case CompleteSupportingEvidenceAnswer(
            supportingEvidences
          ) =>
        Ok(
          checkYourAnswersPage(
            CompleteSupportingEvidenceAnswer(
              supportingEvidences
            ),
            maxUploads,
            isAmend
          )
        )
    }
}

object SupportingEvidenceController {

  final case class ChooseSupportingEvidenceDocumentType(
    supportingEvidenceDocumentType: SupportingEvidenceDocumentType
  )

  val chooseDocumentTypeDataKey = "supporting-evidence.choose-document-type"

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
