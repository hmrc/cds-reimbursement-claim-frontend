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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ScheduledDocumentAnswer, TypeOfClaimAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.{ContactName, Email}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{Eori, GGCredId}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{UploadReference, UpscanUpload}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanFailure
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, SessionData, SignedInUserDetails}

class ScheduleOfMrnDocumentControllerSpec extends FileUploadControllerSpec {

  private lazy val controller = instanceOf[ScheduleOfMrnDocumentController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaim(claim: DraftClaim): (SessionData, FillingOutClaim, DraftClaim) = {
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails = SignedInUserDetails(Some(email), eori, Email(""), ContactName(""))
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      claim
    )
  }

  "The Schedule of MRN document controller" when {

    "handling requests to upload scheduled document" must {

      "show file upload page" when {

        "number of uploads has not exceeded limit" in {
          val (session, _, _) = sessionWithClaim(DraftClaim.blank)

          val uploadReference = sample[UploadReference]
          val upscanUpload    = genUpscanUpload(uploadReference)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.ScheduleOfMrnDocumentController.handleFileSizeErrorCallback(),
              reference => routes.ScheduleOfMrnDocumentController.scanProgress(reference)
            )(Right(upscanUpload))
          }

          checkPageIsDisplayed(
            controller.uploadScheduledDocument()(FakeRequest()),
            messageFromMessageKey("schedule-document.upload.title")
          )
        }
      }

      "show the file upload failed page" when {

        "uploaded file size exceeds threshold" in {
          val (session, _, _) = sessionWithClaim(DraftClaim.blank)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            controller.handleFileSizeErrorCallback()(
              FakeRequest(
                "GET",
                routes.ScheduleOfMrnDocumentController.handleFileSizeErrorCallback().url
              )
            ),
            routes.ScheduleOfMrnDocumentController.showFileSizeErrorPage()
          )
        }
      }

      "show review page" when {
        "document is uploaded" in {
          val scheduledDocument = sample[ScheduledDocumentAnswer]

          val (session, _, _) =
            sessionWithClaim(DraftClaim.blank.copy(scheduledDocumentAnswer = scheduledDocument.some))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            controller.review()(FakeRequest()),
            messageFromMessageKey("schedule-document.review.title")
          )
        }
      }

      "redirect to Select Who Is Making Claim page" when {
        "journey is incomplete" in {
          val scheduledDocument = sample[ScheduledDocumentAnswer]

          val (session, _, _) =
            sessionWithClaim(DraftClaim.blank.copy(scheduledDocumentAnswer = scheduledDocument.some))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            controller.reviewSubmit()(FakeRequest()),
            claimRoutes.SelectWhoIsMakingTheClaimController.selectDeclarantType(JourneyBindable.Scheduled)
          )
        }
      }

      "redirect to Check Your Answers page" when {
        "journey is complete" in {
          val claim = sample(genValidDraftClaim(TypeOfClaimAnswer.Scheduled))

          val (session, _, _) = sessionWithClaim(claim)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            controller.reviewSubmit()(FakeRequest()),
            claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(JourneyBindable.Scheduled)
          )
        }
      }

      "handling requests to delete schedule document" must {
        "redirect to upload scheduled document page" when {
          "removing stored evidence" in {
            val scheduledDocument = sample[ScheduledDocumentAnswer]

            val draftClaim = DraftClaim.blank.copy(scheduledDocumentAnswer = scheduledDocument.some)

            val (session, journey, _) = sessionWithClaim(draftClaim)

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
              controller.deleteScheduledDocument()(FakeRequest()),
              routes.ScheduleOfMrnDocumentController.uploadScheduledDocument()
            )
          }
        }
      }

      "show file format or infected file error page" when {

        "an upscan failure call back is received" in {
          val (session, _, _) = sessionWithClaim(DraftClaim.blank)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            controller.handleFormatOrVirusCheckErrorCallback()(
              FakeRequest()
            ),
            messageFromMessageKey(
              "schedule-document.format-virus-fail.title"
            )
          )
        }
      }

      "handle file format or virus upscan call back" in {
        val uploadReference = sample[UploadReference]
        val upscanFailure   = sample[UpscanFailure]

        val upscanUpload =
          sample[UpscanUpload].copy(
            uploadReference = uploadReference,
            upscanCallBack = Some(upscanFailure)
          )

        val (session, _, _) = sessionWithClaim(DraftClaim.blank)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
        }

        checkIsRedirect(
          controller.scanProgress(uploadReference)(FakeRequest()),
          routes.ScheduleOfMrnDocumentController.handleFormatOrVirusCheckErrorCallback()
        )
      }
    }
  }
}
