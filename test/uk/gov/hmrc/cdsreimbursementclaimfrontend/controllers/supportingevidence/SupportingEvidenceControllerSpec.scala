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
import cats.syntax.all._
import org.scalatest.OptionValues
//import org.scalacheck.Gen
import org.scalamock.handlers.CallHandler3
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._ //{redirectLocation, status, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.SupportingEvidenceController.SupportingEvidenceAnswers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, UUIDGenerator}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceDocumentType.SupportingEvidenceDocumentTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanSuccess //{UploadDetails, UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SupportingEvidenceControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with OptionValues
    with ScalaCheckDrivenPropertyChecks {

  val mockUUIDGenerator: UUIDGenerator = mock[UUIDGenerator]

  val mockUpscanService: UpscanService = mock[UpscanService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[UpscanService].toInstance(mockUpscanService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  lazy val controller: SupportingEvidenceController = instanceOf[SupportingEvidenceController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def mockUpscanInitiate(
    errorRedirectCall: Call,
    successRedirectCall: UploadReference => Call
  )(
    result: Either[Error, UpscanUpload]
  ): CallHandler3[Call, UploadReference => Call, HeaderCarrier, EitherT[Future, Error, UpscanUpload]] =
    (mockUpscanService
      .initiate(_: Call, _: UploadReference => Call)(_: HeaderCarrier))
      .expects(
        where {
          (
            actualErrorRedirectCall: Call,
            actualSuccessRedirectCall: UploadReference => Call,
            _: HeaderCarrier
          ) =>
            val uploadReference = sample[UploadReference]
            actualErrorRedirectCall                    shouldBe errorRedirectCall
            actualSuccessRedirectCall(uploadReference) shouldBe successRedirectCall(uploadReference)
            true
        }
      )
      .returning(EitherT.fromEither[Future](result))

  private def sessionWithClaimState(
    supportingEvidenceAnswers: SupportingEvidenceAnswers
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(supportingEvidenceAnswers = supportingEvidenceAnswers)
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails = SignedInUserDetails(Some(email), eori, Email(""), ContactName(""))
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftC285Claim
    )
  }

//  private def mockGetUpscanUpload(uploadReference: UploadReference)(
//    result: Either[Error, UpscanUpload]
//  ) =
//    (mockUpscanService
//      .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
//      .expects(uploadReference, *)
//      .returning(EitherT.fromEither[Future](result))

  private def genUpscanUpload(uploadReference: UploadReference) = {
    val uploadRequest    = sample[UploadRequest]
    val upscanUploadMeta = UpscanUploadMeta(
      uploadReference.value,
      uploadRequest
    )
    val upscanSuccess    = sample[UpscanSuccess]
    UpscanUpload(
      uploadReference,
      upscanUploadMeta,
      LocalDateTime.now,
      Some(upscanSuccess)
    )
  }

  def testFormError(
    uploadReference: UploadReference,
    data: (String, String)*
  )(
    expectedErrorMessageKey: String,
    errorArgs: Seq[String] = Nil
  )(pageTitleKey: String, titleArgs: String*)(
    performAction: (UploadReference, Seq[(String, String)]) => Future[Result],
    currentSession: SessionData = sessionWithClaimState(
      supportingEvidenceAnswers = sample[SupportingEvidenceAnswers]
    )._1
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }
    checkPageIsDisplayed(
      performAction(uploadReference, data),
      messageFromMessageKey(pageTitleKey, titleArgs: _*),
      { doc =>
        doc
          .select("#error-summary-display > ul > li > a")
          .text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs: _*
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }

  "Supporting Evidence Controller" when {

    "handling requests to upload supporting evidence" must {

      def performAction(): Future[Result] = controller.uploadSupportingEvidence()(FakeRequest())

      "show check your answers page" when {

        "the number of uploads have reached the maximum allowed" in {

          val answers = sample(genSupportingEvidenceAnswersOfN(30))

          val (session, _, _) = sessionWithClaimState(supportingEvidenceAnswers = answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }
      }

      "show technical error page" when {

        "upscan initiate call fails" in {

          val answers = sample[SupportingEvidenceAnswers]

          val (session, _, _) = sessionWithClaimState(supportingEvidenceAnswers = answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.SupportingEvidenceController
                .handleUpscanErrorRedirect(),
              uploadReference =>
                routes.SupportingEvidenceController
                  .scanProgress(uploadReference)
            )(
              Left(Error("some upscan error"))
            )
          }
          checkIsTechnicalErrorPage(performAction())
        }
      }

      "show file upload page" when {

        "number of uploads has not exceeded limit" in {

          val uploadReference = sample[UploadReference]

          val answers = sample(genSupportingEvidenceAnswersOfN(1))

          val (session, _, _) = sessionWithClaimState(answers)

          val upscanUpload = genUpscanUpload(uploadReference)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.SupportingEvidenceController
                .handleUpscanErrorRedirect(),
              uploadReference =>
                routes.SupportingEvidenceController
                  .scanProgress(uploadReference)
            )(Right(upscanUpload))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("supporting-evidence.upload.title"),
            doc =>
              doc
                .select("a.govuk-back-link")
                .attr("href") shouldBe
                uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.routes.BankAccountController
                  .checkBankAccountDetails()
                  .url
          )
        }
      }

      "show the file upload failed page" when {

        "there is an error redirect from upscan" in {
          def performAction(): Future[Result] =
            controller.handleUpscanErrorRedirect()(
              FakeRequest(
                "GET",
                s"/supporting-evidence/handle-upscan-redirect-error?upscan-query-params=some-error-value"
              )
            )

          val answers = sample(genSupportingEvidenceAnswersOfN(2))

          val (session, _, _) = sessionWithClaimState(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          checkIsRedirect(
            performAction(),
            routes.SupportingEvidenceController.documentDidNotUpload()
          )
        }
      }

    }

    "handling requests to display the choose your document type page" must {

      def performAction(uploadReference: UploadReference): Future[Result] =
        controller.chooseSupportingEvidenceDocumentType(uploadReference)(FakeRequest())

      "display the page" when {

        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(documentType = None)
        val answers            = List(supportingEvidence).toNel

        "a user has not answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithClaimState(answers)._1)
          }
          checkPageIsDisplayed(
            performAction(uploadReference),
            messageFromMessageKey(
              "supporting-evidence.choose-document-type.title"
            )
          )
        }

        "a user has answered the question before" in {
          val supportingEvidence =
            sample[SupportingEvidence].copy(documentType = Some(SupportingEvidenceDocumentType.CorrespondenceTrader))

          val answers = List(supportingEvidence).toNel

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithClaimState(answers)._1)
          }

          checkPageIsDisplayed(
            performAction(uploadReference),
            messageFromMessageKey(
              "supporting-evidence.choose-document-type.title"
            )
          )
        }
      }
    }

    "handling requests to chose evidence document type" must {

      def performAction(uploadReference: UploadReference)(data: Seq[(String, String)]): Future[Result] =
        controller.chooseSupportingEvidenceDocumentTypeSubmit(uploadReference)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      def getDocTypeKey(documentType: SupportingEvidenceDocumentType) =
        SupportingEvidenceDocumentTypes.indexOf(documentType)

      "fail" when {
        "document type is missing" in {
          val uploadReference = sample[UploadReference]
          val answers         = sample[SupportingEvidenceAnswers]

          val (session, _, _) = sessionWithClaimState(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction(uploadReference)(Seq.empty)) shouldBe BAD_REQUEST
        }

        "supporting evidence is not found" in {
          val uploadReference = sample[UploadReference]
          val documentTypeKey = getDocTypeKey(sample[SupportingEvidenceDocumentType])

          val (session, _, _) = sessionWithClaimState(supportingEvidenceAnswers = None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsTechnicalErrorPage(
            performAction(uploadReference)(
              Seq(SupportingEvidenceController.chooseDocumentTypeDataKey -> s"$documentTypeKey")
            )
          )
        }

        "an error caught on session update" in {
          val supportingEvidence = sample[SupportingEvidence].copy(documentType = None)
          val answers            = List(supportingEvidence).toNel

          val documentType    = sample[SupportingEvidenceDocumentType]
          val documentTypeKey = getDocTypeKey(documentType)

          val (session, journey, draftClaim) = sessionWithClaimState(answers)

          val updatedSupportingEvidence = supportingEvidence.copy(
            documentType = Some(documentType)
          )

          val updatedAnswers              = List(updatedSupportingEvidence).toNel
          val updatedDraftReturn          = draftClaim.copy(supportingEvidenceAnswers = updatedAnswers)
          val updatedJourney              = journey.copy(draftClaim = updatedDraftReturn)
          val updatedSession: SessionData = session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Left(Error("boom")))
          }

          checkIsTechnicalErrorPage(
            performAction(supportingEvidence.uploadReference)(
              Seq(SupportingEvidenceController.chooseDocumentTypeDataKey -> s"$documentTypeKey")
            )
          )
        }
      }

      "redirect to check your answers page" when {

        "document type is successfully selected" in {
          val supportingEvidence = sample[SupportingEvidence].copy(documentType = None)
          val answers            = List(supportingEvidence).toNel
          val documentType       = sample[SupportingEvidenceDocumentType]
          val documentTypeKey    = getDocTypeKey(documentType)

          val (session, journey, draftClaim) = sessionWithClaimState(answers)

          val updatedSupportingEvidence = supportingEvidence.copy(documentType = Some(documentType))
          val updatedAnswers            = List(updatedSupportingEvidence).toNel

          val updatedDraftReturn          =
            draftClaim.copy(supportingEvidenceAnswers = updatedAnswers)
          val updatedJourney              = journey.copy(draftClaim = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(supportingEvidence.uploadReference)(
              Seq(SupportingEvidenceController.chooseDocumentTypeDataKey -> s"$documentTypeKey")
            ),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }
      }
    }

    "handling requests to delete supporting evidence" must {

      def performAction(uploadReference: UploadReference)(addNew: Boolean): Future[Result] =
        controller.deleteSupportingEvidence(uploadReference, addNew)(FakeRequest())

      "redirect to check your answers page" when {

        "removing already stored evidence" in {
          val supportingEvidence = sample[SupportingEvidence]
          val answers            = List(supportingEvidence).toNel

          val (session, journey, draftClaim) = sessionWithClaimState(answers)

          val updatedDraftReturn          = draftClaim.copy(supportingEvidenceAnswers = None)
          val updatedJourney              = journey.copy(draftClaim = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(supportingEvidence.uploadReference)(addNew = false),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }
      }

      "redirect to upload supporting evidence page" when {

        "removing new evidence" in {
          val answers   = sample(genSupportingEvidenceAnswersOfN(2))
          val evidences = answers.value

          val (session, journey, draftClaim) = sessionWithClaimState(answers)

          val updatedAnswers = evidences.tail.toNel

          val updatedDraftReturn          = draftClaim.copy(supportingEvidenceAnswers = updatedAnswers)
          val updatedJourney              = journey.copy(draftClaim = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(evidences.head.uploadReference)(addNew = true),
            routes.SupportingEvidenceController.uploadSupportingEvidence()
          )
        }
      }

      "show technical error page" when {

        "update session fails" in {
          val evidence = sample[SupportingEvidence]
          val answers  = List(evidence).toNel

          val (session, journey, draftClaim) = sessionWithClaimState(answers)
          val updatedDraftReturn             =
            draftClaim.copy(supportingEvidenceAnswers = None)
          val updatedJourney                 = journey.copy(draftClaim = updatedDraftReturn)
          val updatedSession: SessionData    =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Left(Error("boom")))
          }

          checkIsTechnicalErrorPage(performAction(evidence.uploadReference)(addNew = true))
        }
      }
    }

    /*

    "handling requests to check the upload status of the supporting evidence" must {

      def performAction(uploadReference: UploadReference): Future[Result] =
        controller.scanProgress(uploadReference)(FakeRequest())

      "show technical error page" when {

        "update of draft claim fails" in {
          val supportingEvidence = sample[SupportingEvidence].copy(documentType = None)
          val uploadReference    = supportingEvidence.uploadReference

          val uploadDetails        = sample[UploadDetails].copy(fileName = supportingEvidence.fileName)
          val updatedUpscanSuccess =
            supportingEvidence.upscanSuccess.copy(uploadDetails = uploadDetails)

          val upscanUpload = genUpscanUpload(uploadReference).copy(upscanCallBack = Some(updatedUpscanSuccess))

          val answers = IncompleteSupportingEvidenceAnswer(List(supportingEvidence))

          val (session, journey, draftClaim) = sessionWithClaimState(Some(answers))

          val newSupportingEvidence = SupportingEvidence(
            upscanUpload.uploadReference,
            upscanUpload.upscanUploadMeta,
            upscanUpload.uploadedOn,
            updatedUpscanSuccess,
            updatedUpscanSuccess.fileName,
            None
          )

          val updatedAnswers: IncompleteSupportingEvidenceAnswer =
            answers.copy(evidences = newSupportingEvidence :: answers.evidences)

          val newDraftClaim = draftClaim.fold(
            _.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          )

          val newJourney = journey.copy(draftClaim = newDraftClaim)

          val updatedSession: SessionData = session.copy(journeyStatus = Some(newJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
            mockStoreSession(updatedSession)(Left(Error("boom")))
          }

          checkIsTechnicalErrorPage(performAction(uploadReference))
        }
      }

      "show the document did not upload error page" when {
        "an error redirect from upscan is handled" in {
          def performAction(): Future[Result] =
            controller.documentDidNotUpload()(
              FakeRequest()
            )

          val uploadReference  = sample[UploadReference]
          val uploadRequest    = sample[UploadRequest]
          val upscanUploadMeta = UpscanUploadMeta(
            uploadReference.value,
            uploadRequest
          )
          val upscanSuccess    = sample[UpscanSuccess]

          // we have to do this because atm scalacheck-shapeless cannot generate an instance for a sealed trait with
          // type hierarchy inside a case class
          val supportingEvidence = SupportingEvidence(
            uploadReference,
            upscanUploadMeta,
            LocalDateTime.now(),
            upscanSuccess,
            "file.pdf",
            None
          )

          val answers = CompleteSupportingEvidenceAnswer(
            List(supportingEvidence)
          )

          val (session, _, _) = sessionWithClaimState(Some(answers))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "supporting-evidence.upload-failed.title"
            )
          )
        }
      }

      "show there is a problem with your document error page" when {
        "an upscan failure call back is received" in {
          def performAction(): Future[Result] =
            controller.handleUpscanCallBackFailures()(
              FakeRequest()
            )

          val uploadReference    = sample[UploadReference]
          val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val answers = CompleteSupportingEvidenceAnswer(
            List(supportingEvidence)
          )

          val (session, _, _) = sessionWithClaimState(Some(answers))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "supporting-evidence.scan-failed.title"
            )
          )

        }
      }

      "return the user to the check your answers page" when {

        "the user has completed their upload of supporting evidences" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val answers = CompleteSupportingEvidenceAnswer(
            List(supportingEvidence)
          )

          val (session, _, _) = sessionWithClaimState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(uploadReference),
            routes.SupportingEvidenceController.checkYourAnswers()
          )

        }
      }

      "return the user to the choose document type page" when {

        "the upscan call back came back with a success status" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val upscanSuccess        = sample[UpscanSuccess]
          val uploadDetails        = sample[UploadDetails].copy(fileName = supportingEvidence.fileName)
          val updatedUpscanSuccess = upscanSuccess.copy(uploadDetails = uploadDetails)

          val upscanUpload =
            sample[UpscanUpload].copy(
              uploadReference = uploadReference,
              upscanCallBack = Some(updatedUpscanSuccess)
            )

          val answers = IncompleteSupportingEvidenceAnswer(
            List(supportingEvidence)
          )

          val (session, journey, draftClaim) = sessionWithClaimState(Some(answers))

          val newSupportingEvidence = SupportingEvidence(
            upscanUpload.uploadReference,
            upscanUpload.upscanUploadMeta,
            upscanUpload.uploadedOn,
            updatedUpscanSuccess,
            updatedUpscanSuccess.fileName,
            None
          )

          val updatedAnswers =
            answers.copy(evidences = newSupportingEvidence :: answers.evidences)

          val updatedDraftReturn          =
            draftClaim.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          val updatedJourney              = journey.copy(draftClaim = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(uploadReference),
            routes.SupportingEvidenceController.chooseSupportingEvidenceDocumentType(uploadReference)
          )
        }
      }

      "return the user to scan failed page" when {

        "the upscan call back has not arrived" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val upscanUpload =
            sample[UpscanUpload]
              .copy(uploadReference = uploadReference, upscanCallBack = None)

          val answers = IncompleteSupportingEvidenceAnswer(
            List(supportingEvidence)
          )

          val (session, _, _) = sessionWithClaimState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
          }

          checkPageIsDisplayed(
            performAction(uploadReference),
            messageFromMessageKey(
              "supporting-evidence.scan-progress.title"
            ),
            doc =>
              doc
                .select("#main-content > div > div > h1")
                .text() shouldBe messageFromMessageKey(
                "supporting-evidence.scan-progress.title"
              )
          )
        }

        "the upscan call back came back with a failed status" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val upscanFailure = sample[UpscanFailure]

          val upscanUpload =
            sample[UpscanUpload].copy(
              uploadReference = uploadReference,
              upscanCallBack = Some(upscanFailure)
            )

          val answers = IncompleteSupportingEvidenceAnswer(
            List(supportingEvidence)
          )

          val (session, _, _) = sessionWithClaimState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
          }

          checkIsRedirect(
            performAction(uploadReference),
            routes.SupportingEvidenceController.handleUpscanCallBackFailures()
          )
        }
      }
    }

    "handling actions on check your answer" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      "display the file upload page" when {

        "the user has already answered yes to adding supporting evidences and has not uploaded any evidences so far" in {

          val answers = IncompleteSupportingEvidenceAnswer.empty

          val (session, _, _) = sessionWithClaimState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          def stripUUID(endPoint: String): String =
            endPoint.split("/").dropRight(1).mkString("/")

          val result              = performAction()
          val expectedRedirectUrl =
            routes.SupportingEvidenceController.uploadSupportingEvidence().url

          status(result)                                    shouldBe SEE_OTHER
          redirectLocation(result).map(ep => stripUUID(ep)) shouldBe Some(
            stripUUID(expectedRedirectUrl)
          )
        }
      }

      "display the file upload page" when {

        "the use has never answered this question" in {

          val answers = IncompleteSupportingEvidenceAnswer.empty

          val (session, _, _) = sessionWithClaimState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.SupportingEvidenceController
              .uploadSupportingEvidence()
          )
        }
      }

      "display the check your answers page" when {

        "the user has completed the supporting evidence section" in {

          val answers = CompleteSupportingEvidenceAnswer(
            List.empty
          )

          val (session, _, _) = sessionWithClaimState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "supporting-evidence.check-your-answers.title"
            )
          )
        }

      }

      "display the technical error page" when {

        def performAction(): Future[Result] =
          controller.checkYourAnswersSubmit()(FakeRequest())

        "an error occurs when updating the session with the draft claim" in {

          val answers = IncompleteSupportingEvidenceAnswer.empty

          val updatedAnswers =
            CompleteSupportingEvidenceAnswer(List.empty)

          val (session, journey, draftClaim) = sessionWithClaimState(Some(answers))

          val updatedJourney =
            journey.copy(draftClaim = draftClaim.copy(supportingEvidenceAnswers = Some(updatedAnswers)))

          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Left(Error("boom")))
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

    }
     */
  }

}
