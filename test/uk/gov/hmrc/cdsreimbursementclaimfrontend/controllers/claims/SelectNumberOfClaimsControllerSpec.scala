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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import org.jsoup.nodes.Document
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{BAD_REQUEST, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.collection.JavaConverters._
import scala.concurrent.Future

class SelectNumberOfClaimsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.BulkClaim.enable()

  lazy val errorHandler: ErrorHandler                 = instanceOf[ErrorHandler]
  lazy val controller: SelectNumberOfClaimsController = instanceOf[SelectNumberOfClaimsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def getSessionWithPreviousAnswer(numberOfClaimsType: Option[SelectNumberOfClaimsAnswer]): SessionData = {
    val selectNumberOfClaimsAnswer = numberOfClaimsType
    val draftC285Claim             = DraftC285Claim.newDraftC285Claim.copy(selectNumberOfClaimsAnswer = selectNumberOfClaimsAnswer)
    val ggCredId                   = sample[GGCredId]
    val email                      = sample[Email]
    val eori                       = sample[Eori]
    val signedInUserDetails        = SignedInUserDetails(Some(email), eori, email, ContactName("Anima Amina"))
    val journey                    = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    SessionData.empty.copy(journeyStatus = Some(journey))
  }

  private def updateSession(sessionData: SessionData, numberOfClaimsType: SelectNumberOfClaimsAnswer): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, (draftClaim: DraftC285Claim))) =>
        val newClaim      =
          draftClaim.copy(selectNumberOfClaimsAnswer = Some(numberOfClaimsType))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                         => fail()
    }

  def isIndividualChecked(document: Document): Boolean =
    isChecked(document, "select-number-of-claims-individual")

  def isMultipleChecked(document: Document): Boolean =
    isChecked(document, "select-number-of-claims-bulk")

  def isScheduledChecked(document: Document): Boolean =
    isChecked(document, "select-number-of-claims-scheduled")

  def isChecked(document: Document, fieldId: String): Boolean =
    document
      .getElementById(fieldId)
      .attributes()
      .asList()
      .asScala
      .map(_.getKey)
      .contains("checked")

  def getBackLink(document: Document): String =
    document.select("a.govuk-back-link").attr("href")

  def getErrorSummary(document: Document): String =
    document.select(".govuk-error-summary__list > li > a").text()

  "SelectNumberOfClaimsController" must {

    "redirect to the error page" when {
      "the feature switch bulk claim is disabled" in {
        featureSwitch.BulkClaim.disable()
        val result = controller.show()(FakeRequest())
        status(result) shouldBe NOT_FOUND
      }
    }

    "redirect to the start of the journey" when {
      "there is no journey status in the session" in {
        def performAction(): Future[Result] = controller.show()(FakeRequest())
        val session                         = getSessionWithPreviousAnswer(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.StartController.start()
        )
      }
    }

    "display the page" when {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "the user has not answered this question before" in {
        val session = getSessionWithPreviousAnswer(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-number-of-claims.title"),
          doc => {
            isIndividualChecked(doc) shouldBe false
            isMultipleChecked(doc)   shouldBe false
            isScheduledChecked(doc)  shouldBe false
          }
        )
      }

      "the user has answered this question before and chosen Individual " in {
        val session = getSessionWithPreviousAnswer(Some(SelectNumberOfClaimsAnswer.Individual))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-number-of-claims.title"),
          doc => {
            isIndividualChecked(doc) shouldBe true
            isMultipleChecked(doc)   shouldBe false
            isScheduledChecked(doc)  shouldBe false

          }
        )
      }

      "the user has answered this question before and chosen Multiple " in {
        val session = getSessionWithPreviousAnswer(Some(SelectNumberOfClaimsAnswer.Multiple))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-number-of-claims.title"),
          doc => {
            isIndividualChecked(doc) shouldBe false
            isMultipleChecked(doc)   shouldBe true
            isScheduledChecked(doc)  shouldBe false

          }
        )
      }

      "the user has answered this question before and chosen Scheduled " in {
        val session = getSessionWithPreviousAnswer(Some(SelectNumberOfClaimsAnswer.Scheduled))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-number-of-claims.title"),
          doc => {
            isIndividualChecked(doc) shouldBe false
            isMultipleChecked(doc)   shouldBe false
            isScheduledChecked(doc)  shouldBe true
          }
        )
      }
    }

    "handle submit requests" when {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "user chooses the Individual option" in {
        val session        = getSessionWithPreviousAnswer(None)
        val updatedSession = updateSession(session, SelectNumberOfClaimsAnswer.Individual)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(SelectNumberOfClaimsController.dataKey -> "0")),
          routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single)
        )
      }

      "user chooses the Multiple option" in {
        val session        = getSessionWithPreviousAnswer(None)
        val updatedSession = updateSession(session, SelectNumberOfClaimsAnswer.Multiple)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(SelectNumberOfClaimsController.dataKey -> "1")),
          routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Multiple)
        )
      }

      "user chooses the Scheduled option" in {
        val session        = getSessionWithPreviousAnswer(None)
        val updatedSession = updateSession(session, SelectNumberOfClaimsAnswer.Scheduled)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(SelectNumberOfClaimsController.dataKey -> "2")),
          routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Scheduled)
        )
      }

      "the user amends their previous answer" in {
        val session        = getSessionWithPreviousAnswer(Some(SelectNumberOfClaimsAnswer.Individual))
        val updatedSession = updateSession(session, SelectNumberOfClaimsAnswer.Scheduled)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(SelectNumberOfClaimsController.dataKey -> "2")),
          routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Scheduled)
        )
      }
    }

    "show an error summary" when {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "the user does not select an option and submits the page" in {
        val session = getSessionWithPreviousAnswer(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(Seq.empty),
          messageFromMessageKey("select-number-of-claims.title"),
          getErrorSummary(_) shouldBe messageFromMessageKey(s"select-number-of-claims.error.required"),
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted" in {
        val session = getSessionWithPreviousAnswer(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(Seq(SelectNumberOfClaimsController.dataKey -> "3")),
          messageFromMessageKey("select-number-of-claims.title"),
          getErrorSummary(_) shouldBe messageFromMessageKey(s"select-number-of-claims.invalid"),
          BAD_REQUEST
        )
      }
    }
  }

}
