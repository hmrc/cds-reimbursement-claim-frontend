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

import cats.implicits.catsSyntaxOptionId
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{JourneyBindable, routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ScheduledDocumentAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{UploadDocument, UploadReference, UpscanUpload}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanFailure
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{ContactName, Eori, SessionData, SignedInUserDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

class ScheduleOfMrnDocumentControllerSpec extends FileUploadControllerSpec {

  private lazy val controller             = instanceOf[ScheduleOfMrnDocumentController]
  val featureSwitch: FeatureSwitchService = instanceOf[FeatureSwitchService]

  featureSwitch.EntryNumber.enable()

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    scheduledDocumentAnswer: Option[ScheduledDocumentAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(scheduledDocumentAnswer = scheduledDocumentAnswer)
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails = SignedInUserDetails(Some(email), eori, Email(""), ContactName(""))
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftC285Claim
    )
  }

  "The Schedule of MRN document controller" when {

    "handling requests to upload scheduled document" must {

      "show file upload page" when {

        "number of uploads has not exceeded limit" in {
          val isAmend         = sample[Boolean]
          val uploadReference = sample[UploadReference]
          val (session, _, _) = sessionWithClaimState(None)
          val upscanUpload    = genUpscanUpload(uploadReference)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.ScheduleOfMrnDocumentController.handleFileSizeErrorCallback(isAmend),
              reference => routes.ScheduleOfMrnDocumentController.scanProgress(reference, isAmend)
            )(Right(upscanUpload))
          }

          checkPageIsDisplayed(
            controller.uploadScheduledDocument(isAmend)(FakeRequest()),
            messageFromMessageKey("schedule-document.upload.title")
          )
        }
      }

      "show the file upload failed page" when {

        "uploaded file size exceeds threshold" in {
          val isAmend         = sample[Boolean]
          val (session, _, _) = sessionWithClaimState(None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            controller.handleFileSizeErrorCallback(isAmend)(
              FakeRequest(
                "GET",
                routes.ScheduleOfMrnDocumentController.handleFileSizeErrorCallback(isAmend).url
              )
            ),
            routes.ScheduleOfMrnDocumentController.showFileSizeErrorPage(isAmend)
          )
        }
      }

      "show review page" when {
        "document is uploaded" in {
          val isAmend        = sample[Boolean]
          val uploadDocument = sample[UploadDocument]

          val answer = ScheduledDocumentAnswer(uploadDocument)

          val (session, _, _) = sessionWithClaimState(answer.some)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            controller.review(isAmend)(FakeRequest()),
            messageFromMessageKey("schedule-document.review.title")
          )
        }
      }

      "redirect to Check Your Answers page" when {
        "document is amended" in {
          val uploadDocument = sample[UploadDocument]

          val answer = ScheduledDocumentAnswer(uploadDocument)

          val (session, _, _) = sessionWithClaimState(answer.some)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            controller.reviewSubmit(isAmend = true)(FakeRequest()),
            claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(JourneyBindable.Scheduled)
          )
        }
      }

      "redirect to Select Who Is Making Claim page" when {
        "no document is being amended" in {
          val uploadDocument = sample[UploadDocument]

          val answer = ScheduledDocumentAnswer(uploadDocument)

          val (session, _, _) = sessionWithClaimState(answer.some)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            controller.reviewSubmit(isAmend = false)(FakeRequest()),
            claimRoutes.SelectWhoIsMakingTheClaimController.selectDeclarantType(JourneyBindable.Scheduled)
          )
        }
      }

      "handling requests to delete schedule document" must {
        "redirect to upload scheduled document page" when {
          "removing stored evidence" in {
            val isAmend        = sample[Boolean]
            val uploadDocument = sample[UploadDocument]

            val answer = ScheduledDocumentAnswer(uploadDocument)

            val (session, journey, draftClaim) = sessionWithClaimState(answer.some)

            val updatedDraftReturn = draftClaim.copy(scheduledDocumentAnswer = None)
            val updatedJourney     = journey.copy(draftClaim = updatedDraftReturn)

            val updatedSession: SessionData =
              session.copy(journeyStatus = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              controller.deleteScheduledDocument(isAmend)(FakeRequest()),
              routes.ScheduleOfMrnDocumentController.uploadScheduledDocument(isAmend)
            )
          }
        }
      }

      "show file format or infected file error page" when {

        "an upscan failure call back is received" in {
          val isAmend         = sample[Boolean]
          val (session, _, _) = sessionWithClaimState(None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            controller.handleFormatOrVirusCheckErrorCallback(isAmend)(
              FakeRequest()
            ),
            messageFromMessageKey(
              "schedule-document.format-virus-fail.title"
            )
          )
        }
      }

      "handle file format or virus upscan call back" in {
        val isAmend         = sample[Boolean]
        val uploadReference = sample[UploadReference]
        val upscanFailure   = sample[UpscanFailure]

        val upscanUpload =
          sample[UpscanUpload].copy(
            uploadReference = uploadReference,
            upscanCallBack = Some(upscanFailure)
          )

        val (session, _, _) = sessionWithClaimState(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
        }

        checkIsRedirect(
          controller.scanProgress(uploadReference, isAmend)(FakeRequest()),
          routes.ScheduleOfMrnDocumentController.handleFormatOrVirusCheckErrorCallback(isAmend)
        )
      }
    }
  }
}
