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

import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ScheduledDocumentAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{ContactName, Eori, SessionData, SignedInUserDetails}

import scala.concurrent.Future

class ScheduleOfMrnDocumentControllerSpec extends FileUploadControllerSpec {

  private lazy val controller = instanceOf[ScheduleOfMrnDocumentController]

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

          def performAction(): Future[Result] =
            controller.uploadScheduledDocument()(FakeRequest())

          val uploadReference = sample[UploadReference]
          val (session, _, _) = sessionWithClaimState(None)
          val upscanUpload    = genUpscanUpload(uploadReference)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.ScheduleOfMrnDocumentController.handleFileSizeErrorCallback(),
              routes.ScheduleOfMrnDocumentController.scanProgress
            )(Right(upscanUpload))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("schedule-document.upload.title")
          )
        }
      }

      "show the file upload failed page" when {

        "uploaded file size exceeds threshold" in {
          def performAction(): Future[Result] =
            controller.handleFileSizeErrorCallback()(
              FakeRequest(
                "GET",
                routes.ScheduleOfMrnDocumentController.handleFileSizeErrorCallback().url
              )
            )

          val (session, _, _) = sessionWithClaimState(None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.ScheduleOfMrnDocumentController.showFileSizeErrorPage()
          )
        }
      }
    }
  }
}
