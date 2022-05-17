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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common

import cats.data.EitherT
import cats.implicits.catsStdInstancesForFuture
import org.jsoup.nodes.Document
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.CheckEoriDetailsController.checkEoriDetailsKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.ContactName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.VerifiedEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.CustomsDataStoreService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SelectTypeOfClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckDrivenPropertyChecks {

  val mockCustomsDataStoreService = mock[CustomsDataStoreService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[CustomsDataStoreService].toInstance(mockCustomsDataStoreService)
    )

  lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.RejectedGoods)

  lazy val errorHandler: ErrorHandler              = instanceOf[ErrorHandler]
  lazy val controller: SelectTypeOfClaimController = instanceOf[SelectTypeOfClaimController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def getSessionWithPreviousAnswer(claimType: Option[TypeOfClaimAnswer]): SessionData = {
    val draftC285Claim      = DraftClaim.blank.copy(typeOfClaim = claimType)
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails = SignedInUserDetails(Some(email), eori, email, ContactName("Anima Amina"))
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    SessionData.empty.copy(journeyStatus = Some(journey))
  }

  private def updateSession(sessionData: SessionData, typeOfClaim: TypeOfClaimAnswer): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, draftClaim: DraftClaim)) =>
        val newClaim      =
          draftClaim.copy(typeOfClaim = Some(typeOfClaim))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                   => fail()
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

  private def sessionWithClaimState(): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      = DraftClaim.blank
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails =
      SignedInUserDetails(Some(email), eori, Email("amina@email.com"), ContactName("Fred Bread"))
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftC285Claim
    )
  }

  def getBackLink(document: Document): String =
    document.select("a.govuk-back-link").attr("href")

  def mockGetEmail(response: Either[Error, Option[VerifiedEmail]]) =
    (mockCustomsDataStoreService
      .getEmailByEori(_: Eori)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))
      .once()

  "SelectTypeOfClaimController" must {
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
        val session = getSessionWithPreviousAnswer(Some(TypeOfClaimAnswer.Individual))

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
        val session = getSessionWithPreviousAnswer(Some(TypeOfClaimAnswer.Multiple))

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
        val session = getSessionWithPreviousAnswer(Some(TypeOfClaimAnswer.Scheduled))

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

      val verifiedEmail = "jex.belaran@xmail.com"

      def sessionWithVerifiedUser(session: SessionData): SessionData =
        session.copy(journeyStatus = session.journeyStatus.map { case foc: FillingOutClaim =>
          val siud = foc.signedInUserDetails.copy(verifiedEmail = Email(verifiedEmail))
          foc.copy(signedInUserDetails = siud)
        })

      "user chooses the Individual option" in {
        val session        = getSessionWithPreviousAnswer(None)
        val updatedSession =
          updateSession(sessionWithVerifiedUser(sessionWithVerifiedUser(session)), TypeOfClaimAnswer.Individual)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetEmail(Right(Some(VerifiedEmail(verifiedEmail, ""))))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(SelectTypeOfClaimController.dataKey -> "0")),
          OverpaymentsRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single)
        )
      }

      "user chooses the Multiple option" in {
        val session        = getSessionWithPreviousAnswer(None)
        val updatedSession =
          updateSession(sessionWithVerifiedUser(sessionWithVerifiedUser(session)), TypeOfClaimAnswer.Multiple)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetEmail(Right(Some(VerifiedEmail(verifiedEmail, ""))))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(SelectTypeOfClaimController.dataKey -> "1")),
          OverpaymentsRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Multiple)
        )
      }

      "user chooses the Scheduled option" in {
        val session        = getSessionWithPreviousAnswer(None)
        val updatedSession =
          updateSession(sessionWithVerifiedUser(sessionWithVerifiedUser(session)), TypeOfClaimAnswer.Scheduled)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetEmail(Right(Some(VerifiedEmail(verifiedEmail, ""))))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(SelectTypeOfClaimController.dataKey -> "2")),
          OverpaymentsRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Scheduled)
        )
      }

      "the user amends their previous answer" in {
        val session        = getSessionWithPreviousAnswer(Some(TypeOfClaimAnswer.Individual))
        val updatedSession =
          updateSession(sessionWithVerifiedUser(sessionWithVerifiedUser(session)), TypeOfClaimAnswer.Scheduled)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetEmail(Right(Some(VerifiedEmail(verifiedEmail, ""))))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq(SelectTypeOfClaimController.dataKey -> "2")),
          OverpaymentsRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Scheduled)
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
          performAction(Seq(SelectTypeOfClaimController.dataKey -> "3")),
          messageFromMessageKey("select-number-of-claims.title"),
          getErrorSummary(_) shouldBe messageFromMessageKey(s"select-number-of-claims.invalid"),
          BAD_REQUEST
        )
      }
    }
  }

}
