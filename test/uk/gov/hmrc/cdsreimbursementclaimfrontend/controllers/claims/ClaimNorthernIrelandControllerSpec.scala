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

import org.jsoup.nodes.Document
import org.scalatest.BeforeAndAfterEach
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.ContactName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

import scala.collection.JavaConverters._
import scala.concurrent.Future

class ClaimNorthernIrelandControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TableDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Multiple,
    JourneyBindable.Scheduled
  )

  lazy val errorHandler: ErrorHandler                 = instanceOf[ErrorHandler]
  lazy val controller: ClaimNorthernIrelandController = instanceOf[ClaimNorthernIrelandController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def getSessionWithPreviousAnswer(whetherNorthernIrelandClaim: Option[YesNo]): SessionData = {
    val draftC285Claim      = DraftClaim.blank.copy(whetherNorthernIrelandAnswer = whetherNorthernIrelandClaim)
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails = SignedInUserDetails(Some(email), eori, email, ContactName("Anima Amina"))
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    SessionData.empty.copy(journeyStatus = Some(journey))
  }

  private def updateSession(sessionData: SessionData, whetherNorthernIrelandClaim: YesNo): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, draftClaim: DraftClaim)) =>
        val newClaim      =
          draftClaim.copy(whetherNorthernIrelandAnswer = Some(whetherNorthernIrelandClaim))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                   => fail()
    }

  def isYesChecked(document: Document): Boolean =
    isChecked(document, "claim-northern-ireland-yes")

  def isNoChecked(document: Document): Boolean =
    isChecked(document, "claim-northern-ireland-no")

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

  "ClaimNorthernIrelandController" must {

    "redirect to the start of the journey" when {
      "there is no journey status in the session" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.selectWhetherNorthernIrelandClaim(journey)(FakeRequest())
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
      def performAction(journeyBindable: JourneyBindable): Future[Result] =
        controller.selectWhetherNorthernIrelandClaim(journeyBindable)(FakeRequest())

      "the user has not answered this question before" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey),
          messageFromMessageKey("claim-northern-ireland.title"),
          doc => {
            isYesChecked(doc) shouldBe false
            isNoChecked(doc)  shouldBe false
          }
        )
      }

      "the user has answered this question before and chosen Yes " in forAll(journeys) { journeyBindable =>
        val session = getSessionWithPreviousAnswer(Some(Yes))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journeyBindable),
          messageFromMessageKey("claim-northern-ireland.title"),
          doc => {
            isYesChecked(doc) shouldBe true
            isNoChecked(doc)  shouldBe false
          }
        )
      }

      "the user has answered this question before and chosen No " in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(Some(No))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey),
          messageFromMessageKey("claim-northern-ireland.title"),
          doc => {
            isYesChecked(doc) shouldBe false
            isNoChecked(doc)  shouldBe true
          }
        )
      }

      "the user has come from the CYA page and is amending their answer" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(Some(No))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey),
          messageFromMessageKey("claim-northern-ireland.title"),
          doc => {
            isYesChecked(doc) shouldBe false
            isNoChecked(doc)  shouldBe true
          }
        )

      }

    }

    "handle submit requests" when {

      def performAction(journeyBindable: JourneyBindable, data: Seq[(String, String)]): Future[Result] =
        controller.selectWhetherNorthernIrelandClaimSubmit(journeyBindable)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "user chooses the Yes option" in forAll(journeys) { journey =>
        val session        = getSessionWithPreviousAnswer(None)
        val updatedSession = updateSession(session, Yes)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(journey, Seq(ClaimNorthernIrelandController.dataKey -> "true")),
          routes.SelectBasisForClaimController.selectBasisForClaim(JourneyBindable.Single)
        )
      }

      "user chooses the No option" in forAll(journeys) { journey =>
        val session        = getSessionWithPreviousAnswer(None)
        val updatedSession = updateSession(session, No)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(journey, Seq(ClaimNorthernIrelandController.dataKey -> "false")),
          routes.SelectBasisForClaimController.selectBasisForClaim(JourneyBindable.Single)
        )
      }

      "the user amends their previous answer" in forAll(journeys) { journey =>
        val session        = getSessionWithPreviousAnswer(Some(No))
        val updatedSession = updateSession(session, Yes)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(journey, Seq(ClaimNorthernIrelandController.dataKey -> "true")),
          routes.SelectBasisForClaimController.selectBasisForClaim(JourneyBindable.Single)
        )
      }
    }

    "show an error summary" when {

      def performAction(journeyBindable: JourneyBindable, data: Seq[(String, String)]): Future[Result] =
        controller.selectWhetherNorthernIrelandClaimSubmit(journeyBindable)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "the user does not select an option and submits the page" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey, Seq.empty),
          messageFromMessageKey("claim-northern-ireland.title"),
          getErrorSummary(_) shouldBe messageFromMessageKey(s"claim-northern-ireland.error.required"),
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey, Seq.empty),
          messageFromMessageKey("claim-northern-ireland.title"),
          getErrorSummary(_) shouldBe messageFromMessageKey(s"claim-northern-ireland.error.invalid"),
          BAD_REQUEST
        )
      }
    }
  }

}
