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

import cats.syntax.all._
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, status, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.SupportingEvidenceController.checkYourAnswersDataKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.{ContactName, Email}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyBindableGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{Eori, GGCredId}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.{UploadDetails, UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._

import scala.concurrent.Future

class SupportingEvidenceControllerSpec extends FileUploadControllerSpec {

  private val controller = instanceOf[SupportingEvidenceController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    supportingEvidencesAnswer: Option[SupportingEvidencesAnswer]
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      = DraftClaim.blank.copy(supportingEvidencesAnswer = supportingEvidencesAnswer)
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

  private def testFormError(
    uploadReference: UploadReference,
    data: (String, String)*
  )(
    expectedErrorMessageKey: String,
    errorArgs: Seq[String] = Nil
  )(pageTitleKey: String, titleArgs: String*)(
    performAction: (UploadReference, Seq[(String, String)]) => Future[Result],
    currentSession: SessionData = sessionWithClaimState(
      supportingEvidencesAnswer = Some(sample[SupportingEvidencesAnswer])
    )._1
  ) = {
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

      def performAction(journey: JourneyBindable): Future[Result] =
        controller.uploadSupportingEvidence(journey)(FakeRequest())

      "show technical error page" when {

        "upscan initiate call fails" in {
          val answer          = sample[SupportingEvidencesAnswer]
          val journey         = sample[JourneyBindable]
          val (session, _, _) = sessionWithClaimState(supportingEvidencesAnswer = Some(answer))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.SupportingEvidenceController.handleUpscanErrorRedirect(journey),
              uploadReference => routes.SupportingEvidenceController.scanProgress(journey, uploadReference)
            )(Left(Error("some upscan error")))
          }
          checkIsTechnicalErrorPage(performAction(journey))
        }
      }

      "show file upload page" when {

        "number of uploads has not exceeded limit" in {
          val journey         = sample[JourneyBindable]
          val uploadReference = sample[UploadReference]
          val answer          = sample(arbitrarySupportingEvidencesAnswerOfN(1))
          val (session, _, _) = sessionWithClaimState(answer)
          val upscanUpload    = genUpscanUpload(uploadReference)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.SupportingEvidenceController.handleUpscanErrorRedirect(journey),
              uploadReference => routes.SupportingEvidenceController.scanProgress(journey, uploadReference)
            )(Right(upscanUpload))
          }

          checkPageIsDisplayed(
            performAction(journey),
            messageFromMessageKey("supporting-evidence.upload.title")
          )
        }
      }

      "show the file upload failed page" when {

        "there is an error redirect from upscan" in {

          def performAction(journey: JourneyBindable): Future[Result] =
            controller.handleUpscanErrorRedirect(journey)(
              FakeRequest(
                "GET",
                s"/supporting-evidence/handle-upscan-redirect-error?upscan-query-params=some-error-value"
              )
            )

          val journey         = sample[JourneyBindable]
          val answer          = sample(arbitrarySupportingEvidencesAnswerOfN(2))
          val (session, _, _) = sessionWithClaimState(answer)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          checkIsRedirect(
            performAction(journey),
            routes.SupportingEvidenceController.sizeFail(journey)
          )
        }
      }
    }

    "handling requests to display the choose your document type page" must {

      def performAction(journey: JourneyBindable, uploadReference: UploadReference): Future[Result] =
        controller.chooseSupportingEvidenceDocumentType(journey, uploadReference)(FakeRequest())

      "display the page" when {

        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[UploadDocument].copy(documentType = None)
        val answer             = List(supportingEvidence).toNel

        "a user has not answered the question before" in {
          val journey = sample[JourneyBindable]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithClaimState(answer)._1)
          }
          checkPageIsDisplayed(
            performAction(journey, uploadReference),
            messageFromMessageKey(
              "supporting-evidence.choose-document-type.title"
            )
          )
        }

        "a user has answered the question before" in {
          val journey = sample[JourneyBindable]

          val supportingEvidence =
            sample[UploadDocument].copy(documentType = Some(UploadDocumentType.CorrespondenceTrader))

          val answer = List(supportingEvidence).toNel

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithClaimState(answer)._1)
          }

          checkPageIsDisplayed(
            performAction(journey, uploadReference),
            messageFromMessageKey(
              "supporting-evidence.choose-document-type.title"
            )
          )
        }
      }
    }

    "handling requests to chose evidence document type" must {

      def performAction(journey: JourneyBindable, uploadReference: UploadReference)(
        data: Seq[(String, String)]
      ): Future[Result] =
        controller.chooseSupportingEvidenceDocumentTypeSubmit(journey, uploadReference)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "fail" when {
        "document type is missing" in {
          val journey         = sample[JourneyBindable]
          val uploadReference = sample[UploadReference]
          val answer          = sample[SupportingEvidencesAnswer]

          val (session, _, _) = sessionWithClaimState(Some(answer))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction(journey, uploadReference)(Seq.empty)) shouldBe BAD_REQUEST
        }

        "supporting evidence is not found" in {
          val journey         = sample[JourneyBindable]
          val uploadReference = sample[UploadReference]
          val documentType    = sample[UploadDocumentType]
          val (session, _, _) = sessionWithClaimState(supportingEvidencesAnswer = None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsTechnicalErrorPage(
            performAction(journey, uploadReference)(
              Seq(SupportingEvidenceController.chooseDocumentTypeDataKey -> s"${documentType.index}")
            )
          )
        }

        "an error caught on session update" in {
          val journeyBindable    = sample[JourneyBindable]
          val supportingEvidence = sample[UploadDocument].copy(documentType = None)
          val documentType       = sample[UploadDocumentType]
          val answer             = List(supportingEvidence).toNel

          val (session, journey, draftClaim) = sessionWithClaimState(answer)

          val updatedSupportingEvidence = supportingEvidence.copy(
            documentType = Some(documentType)
          )

          val updatedAnswer               = List(updatedSupportingEvidence).toNel
          val updatedDraftReturn          = draftClaim.copy(supportingEvidencesAnswer = updatedAnswer)
          val updatedJourney              = journey.copy(draftClaim = updatedDraftReturn)
          val updatedSession: SessionData = session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Left(Error("boom")))
          }

          checkIsTechnicalErrorPage(
            performAction(journeyBindable, supportingEvidence.uploadReference)(
              Seq(SupportingEvidenceController.chooseDocumentTypeDataKey -> s"${documentType.index}")
            )
          )
        }
      }

      "redirect to check your answers page" when {

        "document type is successfully selected" in {
          val journeyBindable    = sample[JourneyBindable]
          val supportingEvidence = sample[UploadDocument].copy(documentType = None)
          val documentType       = sample[UploadDocumentType]
          val answer             = List(supportingEvidence).toNel

          val (session, journey, draftClaim) = sessionWithClaimState(answer)

          val updatedSupportingEvidence = supportingEvidence.copy(documentType = Some(documentType))
          val updatedAnswer             = List(updatedSupportingEvidence).toNel

          val updatedDraftReturn          = draftClaim.copy(supportingEvidencesAnswer = updatedAnswer)
          val updatedJourney              = journey.copy(draftClaim = updatedDraftReturn)
          val updatedSession: SessionData = session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(journeyBindable, supportingEvidence.uploadReference)(
              Seq(SupportingEvidenceController.chooseDocumentTypeDataKey -> s"${documentType.index}")
            ),
            routes.SupportingEvidenceController.checkYourAnswers(journeyBindable)
          )
        }
      }

    }

    "handling requests to delete supporting evidence" must {

      def performAction(journey: JourneyBindable, uploadReference: UploadReference)(addNew: Boolean): Future[Result] =
        controller.deleteSupportingEvidence(journey, uploadReference, addNew)(FakeRequest())

      "redirect to check your answers page" when {

        "removing already stored evidence" in {
          val journeyBindable    = sample[JourneyBindable]
          val supportingEvidence = sample[UploadDocument]
          val answers            = List(supportingEvidence).toNel

          val (session, journey, draftClaim) = sessionWithClaimState(answers)

          val updatedDraftReturn = draftClaim.copy(supportingEvidencesAnswer = None)
          val updatedJourney     = journey.copy(draftClaim = updatedDraftReturn)

          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(journeyBindable, supportingEvidence.uploadReference)(addNew = false),
            routes.SupportingEvidenceController.checkYourAnswers(journeyBindable)
          )
        }
      }

      "redirect to upload supporting evidence page" when {

        "removing new evidence" in {
          val journeyBindable = sample[JourneyBindable]
          val answer          = sample(arbitrarySupportingEvidencesAnswerOfN(2))

          val (session, journey, draftClaim) = sessionWithClaimState(answer)

          val updatedAnswer = answer.flatMap(_.tail.toNel)

          val updatedDraftReturn          = draftClaim.copy(supportingEvidencesAnswer = updatedAnswer)
          val updatedJourney              = journey.copy(draftClaim = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(journeyBindable, answer.map(_.head.uploadReference).value)(addNew = true),
            routes.SupportingEvidenceController.uploadSupportingEvidence(journeyBindable)
          )
        }
      }

      "show technical error page" when {

        "update session fails" in {
          val journeyBindable = sample[JourneyBindable]
          val evidence        = sample[UploadDocument]
          val answer          = List(evidence).toNel

          val (session, journey, draftClaim) = sessionWithClaimState(answer)

          val updatedDraftReturn          = draftClaim.copy(supportingEvidencesAnswer = None)
          val updatedJourney              = journey.copy(draftClaim = updatedDraftReturn)
          val updatedSession: SessionData = session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Left(Error("boom")))
          }

          checkIsTechnicalErrorPage(performAction(journeyBindable, evidence.uploadReference)(addNew = true))
        }
      }
    }

    "handling requests to check the upload status of the supporting evidence" must {

      def performAction(journeyBindable: JourneyBindable, uploadReference: UploadReference): Future[Result] =
        controller.scanProgress(journeyBindable, uploadReference)(FakeRequest())

      "show technical error page" when {

        "update of draft claim fails" in {
          val journeyBindable    = sample[JourneyBindable]
          val supportingEvidence = sample[UploadDocument].copy(documentType = None)
          val uploadReference    = supportingEvidence.uploadReference

          val uploadDetails        = sample[UploadDetails].copy(fileName = supportingEvidence.fileName)
          val updatedUpscanSuccess =
            supportingEvidence.upscanSuccess.copy(uploadDetails = uploadDetails)

          val upscanUpload = genUpscanUpload(uploadReference).copy(upscanCallBack = Some(updatedUpscanSuccess))

          val answer = sample(arbitrarySupportingEvidencesAnswerOpt)

          val (session, journey, draftClaim) = sessionWithClaimState(answer)

          val newSupportingEvidence = UploadDocument(
            upscanUpload.uploadReference,
            upscanUpload.upscanUploadMeta,
            upscanUpload.uploadedOn,
            updatedUpscanSuccess,
            updatedUpscanSuccess.fileName,
            None
          )

          val updatedAnswer = answer.map(_ :+ newSupportingEvidence) orElse List(newSupportingEvidence).toNel

          val newDraftClaim = draftClaim.copy(supportingEvidencesAnswer = updatedAnswer)

          val newJourney = journey.copy(draftClaim = newDraftClaim)

          val updatedSession: SessionData = session.copy(journeyStatus = Some(newJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
            mockStoreSession(updatedSession)(Left(Error("boom")))
          }

          checkIsTechnicalErrorPage(performAction(journeyBindable, uploadReference))
        }
      }

      "show the document did not upload error page" when {
        "an error redirect from upscan is handled" in {
          def performAction(journey: JourneyBindable): Future[Result] =
            controller.sizeFail(journey)(
              FakeRequest()
            )

          val journey         = sample[JourneyBindable]
          val answer          = sample[SupportingEvidencesAnswer]
          val (session, _, _) = sessionWithClaimState(Some(answer))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(journey),
            messageFromMessageKey(
              "supporting-evidence.upload-failed.title"
            )
          )
        }
      }

      "show there is a problem with your document error page" when {
        "an upscan failure call back is received" in {
          def performAction(journey: JourneyBindable): Future[Result] =
            controller.handleUpscanCallBackFailures(journey)(
              FakeRequest()
            )

          val journey = sample[JourneyBindable]
          val answer  = sample[SupportingEvidencesAnswer]

          val (session, _, _) = sessionWithClaimState(Some(answer))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(journey),
            messageFromMessageKey(
              "supporting-evidence.scan-failed.title"
            )
          )
        }
      }

      "discard adding duplicate evidence with the same reference" when {

        "user revisits page" in {
          val journey  = sample[JourneyBindable]
          val evidence = sample[UploadDocument]
          val answers  = List(evidence).toNel

          val upscanUpload =
            sample[UpscanUpload].copy(
              uploadReference = evidence.uploadReference,
              upscanCallBack = Some(evidence.upscanSuccess)
            )

          val (session, _, _) = sessionWithClaimState(answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(evidence.uploadReference)(Right(upscanUpload))
          }

          checkIsRedirect(
            performAction(journey, evidence.uploadReference),
            routes.SupportingEvidenceController
              .chooseSupportingEvidenceDocumentType(journey, evidence.uploadReference)
          )
        }
      }

      "return the user to the choose document type page" when {

        "the upscan call back came back with a success status" in {
          val journeyBindable      = sample[JourneyBindable]
          val upscanSuccess        = sample[UpscanSuccess]
          val uploadDetails        = sample[UploadDetails]
          val updatedUpscanSuccess = upscanSuccess.copy(uploadDetails = uploadDetails)

          val uploadReference = sample[UploadReference]
          val answer          = sample(arbitrarySupportingEvidencesAnswerOpt)

          val (session, journey, draftClaim) = sessionWithClaimState(answer)

          val upscanUpload =
            sample[UpscanUpload].copy(
              uploadReference = uploadReference,
              upscanCallBack = Some(updatedUpscanSuccess)
            )

          val newSupportingEvidence = UploadDocument(
            upscanUpload.uploadReference,
            upscanUpload.upscanUploadMeta,
            upscanUpload.uploadedOn,
            updatedUpscanSuccess,
            updatedUpscanSuccess.fileName,
            None
          )

          val updatedAnswer = answer.map(_ :+ newSupportingEvidence) orElse List(newSupportingEvidence).toNel

          val updatedDraftReturn          = draftClaim.copy(supportingEvidencesAnswer = updatedAnswer)
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
            performAction(journeyBindable, uploadReference),
            routes.SupportingEvidenceController.chooseSupportingEvidenceDocumentType(journeyBindable, uploadReference)
          )
        }
      }

      "return the user to scan failed page" when {

        "the upscan call back has not arrived" in {
          val journey            = sample[JourneyBindable]
          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[UploadDocument].copy(uploadReference = uploadReference)

          val upscanUpload =
            sample[UpscanUpload]
              .copy(uploadReference = uploadReference, upscanCallBack = None)

          val answer = List(supportingEvidence).toNel

          val (session, _, _) = sessionWithClaimState(answer)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
          }

          checkPageIsDisplayed(
            performAction(journey, uploadReference),
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
          val journey            = sample[JourneyBindable]
          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[UploadDocument].copy(uploadReference = uploadReference)

          val upscanFailure = sample[UpscanFailure]

          val upscanUpload =
            sample[UpscanUpload].copy(
              uploadReference = uploadReference,
              upscanCallBack = Some(upscanFailure)
            )

          val answer = List(supportingEvidence).toNel

          val (session, _, _) = sessionWithClaimState(answer)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
          }

          checkIsRedirect(
            performAction(journey, uploadReference),
            routes.SupportingEvidenceController.handleUpscanCallBackFailures(journey)
          )
        }
      }
    }

    "handling actions on check your answer" must {

      def performAction(journey: JourneyBindable): Future[Result] =
        controller.checkYourAnswers(journey)(FakeRequest())

      "display the file upload page" when {

        "the user has already answered yes to adding supporting evidences and has not uploaded any evidences so far" in {
          val journey         = sample[JourneyBindable]
          val (session, _, _) = sessionWithClaimState(supportingEvidencesAnswer = None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          def stripUUID(endPoint: String): String =
            endPoint.split("/").dropRight(1).mkString("/")

          val result              = performAction(journey)
          val expectedRedirectUrl =
            routes.SupportingEvidenceController.uploadSupportingEvidence(journey).url

          status(result)                                    shouldBe SEE_OTHER
          redirectLocation(result).map(ep => stripUUID(ep)) shouldBe Some(
            stripUUID(expectedRedirectUrl)
          )
        }

        "the use has never answered this question" in {
          val journey         = sample[JourneyBindable]
          val (session, _, _) = sessionWithClaimState(supportingEvidencesAnswer = None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(journey),
            routes.SupportingEvidenceController.uploadSupportingEvidence(journey)
          )
        }
      }

      "show the count of uploaded documents in the title" when {

        "displaying the check your answers page" in {
          val journey = sample[JourneyBindable]
          val answer  = sample[SupportingEvidencesAnswer]

          val (session, _, _) = sessionWithClaimState(Some(answer))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(journey),
            messageFromMessageKey(
              "supporting-evidence.check-your-answers.title",
              answer.size,
              if (answer.size > 1) "s" else ""
            )
          )
        }
      }
    }

    "submitting supporting evidence answers" must {

      def performAction(journey: JourneyBindable, whetherAddNewDocument: Option[Boolean]): Future[Result] =
        controller.checkYourAnswersSubmit(journey)(
          FakeRequest()
            .withFormUrlEncodedBody(checkYourAnswersDataKey -> whetherAddNewDocument.fold("")(_.toString))
        )

      "redirect to check all your answers page" when {

        "user responded do not add more documents" in {
          val journey = sample[JourneyBindable]
          val answer  = sample[SupportingEvidencesAnswer]

          val (session, _, _) = sessionWithClaimState(Some(answer))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(journey, whetherAddNewDocument = Some(false)),
            claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journey)
          )
        }
      }

      "redirect to upload new document page" when {

        "user decided to add more documents" in {
          val journey = sample[JourneyBindable]
          val answer  = sample[SupportingEvidencesAnswer]

          val (session, _, _) = sessionWithClaimState(Some(answer))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(journey, whetherAddNewDocument = Some(true)),
            routes.SupportingEvidenceController.uploadSupportingEvidence(journey)
          )
        }
      }

      "show validation error if answer is undefined" in {
        val journey = sample[JourneyBindable]
        val answer  = sample[SupportingEvidencesAnswer]

        val (session, _, _) = sessionWithClaimState(Some(answer))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        status(performAction(journey, whetherAddNewDocument = None)) should be(BAD_REQUEST)
      }

      "redirect to upload evidence page if not answers exist" in {
        val journey = sample[JourneyBindable]

        val (session, _, _) = sessionWithClaimState(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(journey, whetherAddNewDocument = None),
          routes.SupportingEvidenceController.uploadSupportingEvidence(journey)
        )
      }
    }
  }

}
