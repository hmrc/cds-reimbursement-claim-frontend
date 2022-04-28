/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.syntax.all._
import org.scalatest.OptionValues
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.SupportingEvidenceController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyBindableGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

import scala.concurrent.Future

class ChooseFileTypeControllerSpec extends ControllerSpec with AuthSupport with SessionSupport with OptionValues {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: ChooseFileTypeController = instanceOf[ChooseFileTypeController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    documentTypeAnswer: Option[UploadDocumentType]
  ): SessionData = {
    val draftC285Claim      =
      DraftClaim.blank.copy(
        documentTypeAnswer = documentTypeAnswer
      )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)

    SessionData.empty.copy(
      journeyStatus = Some(journey)
    )

  }

  val pageKey: String = "supporting-evidence.choose-document-type"

  "handling requests to display the choose your document type page" must {

    def performAction(journey: JourneyBindable): Future[Result] =
      controller.chooseSupportingEvidenceDocumentType(journey)(FakeRequest())

    "display the page" when {

      val answer = sample[UploadDocumentType]

      "a user has not answered the question before" in {
        val journey = sample[JourneyBindable]
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithClaimState(None))
        }
        checkPageIsDisplayed(
          performAction(journey),
          messageFromMessageKey(s"$pageKey.title"),
          assertThatNoneRadioButtonIsSelected
        )
      }

      "a user has answered the question before" in {
        val journey = sample[JourneyBindable]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithClaimState(answer.some))
        }

        checkPageIsDisplayed(
          performAction(journey),
          messageFromMessageKey(s"$pageKey.title"),
          assertThatNoneRadioButtonIsSelected
        )
      }
    }
  }

  "handling requests to chose evidence document type" must {

    def performAction(journey: JourneyBindable)(
      data: Seq[(String, String)]
    ): Future[Result] =
      controller.chooseSupportingEvidenceDocumentTypeSubmit(journey)(
        FakeRequest().withFormUrlEncodedBody(data: _*)
      )

    "redisplay the page" when {
      "document type is missing" in {
        val journey = sample[JourneyBindable]
        val answer  = sample[UploadDocumentType]

        val session = sessionWithClaimState(answer.some)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey)(Seq.empty),
          messageFromMessageKey(s"$pageKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$pageKey.error.required"
            ),
          BAD_REQUEST
        )
      }

      "document type not found" in {
        val journey = sample[JourneyBindable]
        val answer  = sample[UploadDocumentType]

        val session = sessionWithClaimState(answer.some)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey)(
            Seq(SupportingEvidenceController.chooseDocumentTypeDataKey -> "FOO")
          ),
          messageFromMessageKey(s"$pageKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$pageKey.error.invalid-document-type"
            ),
          BAD_REQUEST
        )
      }

      "an error caught on session update" in {
        val journeyBindable = sample[JourneyBindable]
        val answer          = sample[UploadDocumentType]

        val session = sessionWithClaimState(None)

        val updatedSession: SessionData =
          session.withUpdatedC285Claim(_.copy(documentTypeAnswer = answer.some))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Left(Error("boom")))
        }

        checkIsTechnicalErrorPage(
          performAction(journeyBindable)(
            Seq(SupportingEvidenceController.chooseDocumentTypeDataKey -> UploadDocumentType.keyOf(answer))
          )
        )
      }
    }

    "redirect to file selection page" when {

      "document type is successfully selected" in {
        val journeyBindable = sample[JourneyBindable]
        val answer          = sample[UploadDocumentType]

        val session = sessionWithClaimState(None)

        val updatedSession: SessionData =
          session.withUpdatedC285Claim(_.copy(documentTypeAnswer = answer.some))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(journeyBindable)(
            Seq(SupportingEvidenceController.chooseDocumentTypeDataKey -> UploadDocumentType.keyOf(answer))
          ),
          fileUploadRoutes.SupportingEvidenceController.uploadSupportingEvidence(journeyBindable)
        )
      }
    }

  }

}
