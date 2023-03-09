/*
 * Copyright 2023 HM Revenue & Customs
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
import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.Name
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.CheckEoriDetailsController.checkEoriDetailsKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.ContactName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.VerifiedEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.VerifiedEmailAddressService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.mvc.Call

class CheckEoriDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  lazy val featureSwitch = instanceOf[FeatureSwitchService]

  val mockVerifiedEmailAddressService = mock[VerifiedEmailAddressService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[VerifiedEmailAddressService].toInstance(mockVerifiedEmailAddressService)
    )

  lazy val controller: CheckEoriDetailsController = instanceOf[CheckEoriDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      = DraftClaim.blank
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails =
      SignedInUserDetails(Some(email), eori, Email("amina@email.com"), ContactName("Fred Bread"))
    val journey             =
      FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftC285Claim
    )
  }

  def getBackLink(document: Document): String =
    document.select("a.govuk-back-link").attr("href")

  def mockGetEmail(response: Either[Error, Option[VerifiedEmail]]) =
    (mockVerifiedEmailAddressService
      .getVerifiedEmailAddress(_: Eori)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))
      .once()

  def mockAuthRequiredRetrievals(eori: Eori, name: contactdetails.Name) =
    mockAuthWithAllRetrievals(
      Some(AffinityGroup.Individual),
      Some("email"),
      Set(Enrolment("HMRC-CUS-ORG", Seq(EnrolmentIdentifier("EORINumber", eori.value)), "Activated", None)),
      Some(Credentials("credId", "GovernmentGateway")),
      Some(Name(name.name, name.lastName))
    )

  def mockC285AuthRequiredRetrievals(eori: Eori, contactName: ContactName) =
    mockAuthRequiredRetrievals(eori, contactdetails.Name(Some(contactName.value), None))

  "Check Eori Details Controller" must {

    "redirect to the start of the journey" when {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "there is no journey status in the session" in {
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockC285AuthRequiredRetrievals(
            fillingOutClaim.signedInUserDetails.eori,
            fillingOutClaim.signedInUserDetails.contactName
          )
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.StartController.start()
        )
      }
    }

    "Render the page" when {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "The user is logged into a C285 journey" in forAll { (eori: Eori, name: contactdetails.Name) =>
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockAuthRequiredRetrievals(eori, name)
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("check-eori-details.title"),
          doc => {
            doc.select("form dl div dd").get(0).text() shouldBe fillingOutClaim.signedInUserDetails.eori.value
            doc.select("form dl div dd").get(1).text() shouldBe fillingOutClaim.signedInUserDetails.contactName.value
          }
        )
      }

      "The user is logged in with a C&E1179 journey" in forAll { (eori: Eori, name: contactdetails.Name) =>
        val session = SessionData(RejectedGoodsSingleJourney.empty(eori))

        inSequence {
          mockAuthRequiredRetrievals(eori, name)
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("check-eori-details.title"),
          doc => {
            doc.select("form dl div dd").get(0).text() shouldBe eori.value
            doc.select("form dl div dd").get(1).text() shouldBe name.name.get
          }
        )
      }
    }
    "Handle submissions" should {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val verifiedEmail = "foo@bar.com"

      "Redirect to ChooseClaimTypeController if user says details are correct and email address is verified" in {
        featureSwitch.disable(Feature.RejectedGoods)
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockC285AuthRequiredRetrievals(
            fillingOutClaim.signedInUserDetails.eori,
            fillingOutClaim.signedInUserDetails.contactName
          )
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
          mockGetEmail(Right(Some(VerifiedEmail(verifiedEmail, ""))))
        }

        val result = performAction(Seq(checkEoriDetailsKey -> "true"))
        checkIsRedirect(result, commonRoutes.ChooseClaimTypeController.show())
      }

      "Redirect to the email frontend if user says details are correct but email address not verified" in {
        featureSwitch.disable(Feature.RejectedGoods)
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockC285AuthRequiredRetrievals(
            fillingOutClaim.signedInUserDetails.eori,
            fillingOutClaim.signedInUserDetails.contactName
          )
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
          mockGetEmail(Right(None))
        }

        val result = performAction(Seq(checkEoriDetailsKey -> "true"))
        checkIsRedirect(result, Call("GET", "http://localhost:9898/manage-email-cds/service/cds-reimbursement-claim"))
      }

      "Display an error page if user says details are correct but email address verification fails" in {
        featureSwitch.disable(Feature.RejectedGoods)
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockC285AuthRequiredRetrievals(
            fillingOutClaim.signedInUserDetails.eori,
            fillingOutClaim.signedInUserDetails.contactName
          )
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
          mockGetEmail(Left(Error("Cannot verify email address")))
        }

        val result = performAction(Seq(checkEoriDetailsKey -> "true"))
        checkIsTechnicalErrorPage(result)
      }

      "Redirect to signout if the user chooses the Eori is incorrect, logout option" in {
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockC285AuthRequiredRetrievals(
            fillingOutClaim.signedInUserDetails.eori,
            fillingOutClaim.signedInUserDetails.contactName
          )
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        val result = performAction(Seq(checkEoriDetailsKey -> "false"))
        checkIsRedirect(result, viewConfig.ggSignOut)
      }

      "The user submits an invalid choice" in {
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockC285AuthRequiredRetrievals(
            fillingOutClaim.signedInUserDetails.eori,
            fillingOutClaim.signedInUserDetails.contactName
          )
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        val result = performAction(Seq())

        checkPageIsDisplayed(
          result,
          messageFromMessageKey("check-eori-details.title"),
          _.select("#check-eori-details-error")
            .html() shouldBe "<span class=\"govuk-visually-hidden\">Error:</span> " + messageFromMessageKey(
            s"check-eori-details.error.invalid"
          ),
          BAD_REQUEST
        )
      }

      "The user submits no choice" in {
        val (session, fillingOutClaim, _) = sessionWithClaimState()

        inSequence {
          mockC285AuthRequiredRetrievals(
            fillingOutClaim.signedInUserDetails.eori,
            fillingOutClaim.signedInUserDetails.contactName
          )
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        val result = performAction(Seq.empty)

        checkPageIsDisplayed(
          result,
          messageFromMessageKey(s"$checkEoriDetailsKey.title"),
          _.select("#check-eori-details-error")
            .html() shouldBe "<span class=\"govuk-visually-hidden\">Error:</span> " + messageFromMessageKey(
            s"check-eori-details.error.invalid"
          ),
          BAD_REQUEST
        )
      }
    }
  }
}
