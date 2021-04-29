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

import cats.data.EitherT
import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{CustomsDataStoreService, FeatureSwitchService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckEoriDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  lazy val featureSwitch                               = instanceOf[FeatureSwitchService]

  val mockCustomsDataStoreService                      = mock[CustomsDataStoreService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[CustomsDataStoreService].toInstance(mockCustomsDataStoreService)
    )

  lazy val controller: CheckEoriDetailsController = instanceOf[CheckEoriDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim
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

  "Check Eori Details Controller" must {

    val verifiedEmail = "jex.belaran@xmail.com"

    "redirect to the start of the journey" when {
      "there is no journey status in the session" in {
        def performAction(): Future[Result] = controller.show()(FakeRequest())
        val (session, _, _)                 = sessionWithClaimState()

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

    "Render the page" when {
      "The user is logged in" in {
        def performAction(): Future[Result] = controller.show()(FakeRequest())

        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("check-eori-details.title"),
          doc => getBackLink(doc) shouldBe baseRoutes.LandingPageController.landing().url
        )
      }
    }

    "Handle submissions" should {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "Redirect to SelectNumberOfClaims if user says details are correct and FeatureSwitch.Bulk is enabled" in {
        featureSwitch.BulkClaim.enable()
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
          mockGetEmail(Right(Some(VerifiedEmail(verifiedEmail, ""))))
          mockStoreSession(Right(()))
        }

        val result = performAction(Seq(CheckEoriDetailsController.dataKey -> "0"))
        checkIsRedirect(result, routes.SelectNumberOfClaimsController.show(false))
      }

      "Redirect to EnterMovementReferenceNumber if user says details are correct and FeatureSwitch.Bulk is disabled" in {
        featureSwitch.BulkClaim.disable()
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
          mockGetEmail(Right(Some(VerifiedEmail(verifiedEmail, ""))))
          mockStoreSession(Right(()))
        }

        val result = performAction(Seq(CheckEoriDetailsController.dataKey -> "0"))
        checkIsRedirect(result, routes.EnterMovementReferenceNumberController.enterMrn())
      }

      "Redirect to signout if the user chooses the Eori is incorrect, logout option" in {
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        val result = performAction(Seq(CheckEoriDetailsController.dataKey -> "1"))
        checkIsRedirect(result, viewConfig.ggSignOut)
      }

      "The user submits an invalid choice" in {
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        val result = performAction(Seq(CheckEoriDetailsController.dataKey -> "2"))

        checkPageIsDisplayed(
          result,
          messageFromMessageKey("check-eori-details.title"),
          _.select("#check-eori-details-error").text() shouldBe "Error: " + messageFromMessageKey(
            s"check-eori-details.invalid"
          ),
          BAD_REQUEST
        )
      }

      "The user submits no choice" in {
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        val result = performAction(Seq.empty)

        checkPageIsDisplayed(
          result,
          messageFromMessageKey("check-eori-details.title"),
          _.select("#check-eori-details-error").text() shouldBe "Error: " + messageFromMessageKey(
            s"check-eori-details.invalid"
          ),
          BAD_REQUEST
        )
      }

    }

  }
}
