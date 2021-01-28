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
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionStore
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SupportingEvidenceAnswers.{CompleteSupportingEvidenceAnswers, IncompleteSupportingEvidenceAnswers, SupportingEvidence}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, upscan => _, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{supportingevidence => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SupportingEvidenceController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  upscanService: UpscanService,
  cc: MessagesControllerComponents,
  val config: Configuration,
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
      SupportingEvidenceAnswers
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
          f(s, r, IncompleteSupportingEvidenceAnswers.empty)
        )(f(s, r, _))

      case _ => Redirect(baseRoutes.StartController.start())
    }

  def uploadSupportingEvidence(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, _, answers) =>
        if (answers.fold(_.evidences, _.evidences).length >= maxUploads)
          Redirect(routes.SupportingEvidenceController.checkYourAnswers())
        else
          upscanService
            .initiate(
              routes.SupportingEvidenceController
                .handleUpscanErrorRedirect(),
              routes.SupportingEvidenceController.scanProgress
            )
            .fold(
              //FIXME
              _ =>
                //logger.warn("could not start upload supporting evidence", e)
                errorHandler.errorResult(),
              uploadUpscan =>
                Ok(
                  uploadPage(
                    uploadUpscan,
                    baseRoutes.BankAccountController.checkBankAccountDetails()
                  )
                )
            )
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
          case _: CompleteSupportingEvidenceAnswers =>
            Redirect(routes.SupportingEvidenceController.checkYourAnswers())

          case incompleteAnswers: IncompleteSupportingEvidenceAnswers =>
            val result = for {
              upscanUpload <- upscanService.getUpscanUpload(uploadReference)
              _            <- upscanUpload.upscanCallBack match {
                                case Some(s: UpscanSuccess) =>
                                  storeUpscanSuccess(
                                    upscanUpload,
                                    s,
                                    incompleteAnswers,
                                    fillingOutReturn
                                  )
                                case _                      =>
                                  EitherT.pure[Future, Error](())
                              }
            } yield upscanUpload

            result.fold(
              e => {
                logger.warn(
                  s"could not update the status of upscan upload to uploaded : $e"
                )
                errorHandler.errorResult()
              },
              upscanUpload =>
                upscanUpload.upscanCallBack match {
                  case Some(_: UpscanSuccess) =>
                    Redirect(
                      routes.SupportingEvidenceController.checkYourAnswers()
                    )
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
    answers: IncompleteSupportingEvidenceAnswers,
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
              success.fileName
            )
          answers.copy(evidences = supportingEvidence :: answers.evidences)

      }

    val newDraftClaim = fillingOutClaim.draftClaim.fold(
      _.copy(supportingEvidenceAnswers = Some(newAnswers))
    )
    val newJourney    = fillingOutClaim.copy(draftClaim = newDraftClaim)

    //TODO: determine if we need to store the claim at this point
    EitherT(
      updateSession(sessionStore, request)(
        _.copy(journeyStatus = Some(newJourney))
      )
    )
  }

  def deleteSupportingEvidence(
    uploadReference: UploadReference,
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
            IncompleteSupportingEvidenceAnswers(
              newEvidences
            )
          }
        )

        val newDraftClaim = fillingOutClaim.draftClaim.fold(
          _.copy(supportingEvidenceAnswers = Some(updatedAnswers))
        )
        val newJourney    = fillingOutClaim.copy(draftClaim = newDraftClaim)

        val result = for {
          //TODO: determine if we need to store the draft claim at this point
          _ <- EitherT(
                 updateSession(sessionStore, request)(
                   _.copy(journeyStatus = Some(newJourney))
                 )
               )
        } yield ()

        result.fold(
          //FIXME
          _ =>
            //logger.warn("Could not update session", e)
            errorHandler.errorResult(),
          _ =>
            if (addNew)
              Redirect(
                routes.SupportingEvidenceController.uploadSupportingEvidence()
              )
            else
              Redirect(routes.SupportingEvidenceController.checkYourAnswers())
        )
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, _, answers) =>
        checkYourAnswersHandler(answers)
      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withUploadSupportingEvidenceAnswers { (_, fillingOutClaim, answers) =>
        val updatedAnswers: SupportingEvidenceAnswers = answers match {
          case IncompleteSupportingEvidenceAnswers(
                evidences
              ) =>
            CompleteSupportingEvidenceAnswers(
              evidences
            )
          case CompleteSupportingEvidenceAnswers(
                evidences
              ) =>
            CompleteSupportingEvidenceAnswers(
              evidences
            )
        }

        val newDraftClaim = fillingOutClaim.draftClaim.fold(
          _.copy(supportingEvidenceAnswers = Some(updatedAnswers))
        )
        val newJourney    = fillingOutClaim.copy(draftClaim = newDraftClaim)

        val result = for {
          //TODO: determine if we need to store the draft claim at this point
          _ <- EitherT(
                 updateSession(sessionStore, request)(
                   _.copy(journeyStatus = Some(newJourney))
                 )
               )
        } yield ()

        result.fold(
          //FIXME
          _ =>
            //logger.warn("Could not update session", e)
            errorHandler.errorResult(),
          _ => Redirect(claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers())
        )
      }
    }

  private def checkYourAnswersHandler(
    answers: SupportingEvidenceAnswers
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {

      case IncompleteSupportingEvidenceAnswers(
            supportingEvidences
          ) if supportingEvidences.isEmpty =>
        Redirect(routes.SupportingEvidenceController.uploadSupportingEvidence())

      case IncompleteSupportingEvidenceAnswers(
            supportingEvidences
          ) =>
        Ok(
          checkYourAnswersPage(
            CompleteSupportingEvidenceAnswers(supportingEvidences),
            maxUploads
          )
        )

      case CompleteSupportingEvidenceAnswers(
            supportingEvidences
          ) =>
        Ok(
          checkYourAnswersPage(
            CompleteSupportingEvidenceAnswers(
              supportingEvidences
            ),
            maxUploads
          )
        )
    }
}
