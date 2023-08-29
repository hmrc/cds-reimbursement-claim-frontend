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

import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Call
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.session
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.Name
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.CheckEoriDetailsController.checkEoriDetailsKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.CdsVerifiedEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.ContactName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.VerifiedEmailAddressService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney

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

  def getBackLink(document: Document): String =
    document.select("a.govuk-back-link").attr("href")

  def mockGetEmail(response: Either[Error, Option[CdsVerifiedEmail]]) =
    (mockVerifiedEmailAddressService
      .getVerifiedEmailAddress(_: Eori)(_: HeaderCarrier))
      .expects(*, *)
      .returning(Future.successful(response))
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

      "user does not have CDS enrolment" in {
        inSequence {
          mockAuthWithNonGGUserRetrievals()
          mockGetSession(SessionData.empty)
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.StartController.start()
        )
      }
    }

    "Render the page" when {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "The user is logged with CDS enrolment" in forAll { (eori: Eori, name: contactdetails.Name) =>
        inSequence {
          mockAuthRequiredRetrievals(eori, name)
          mockGetSession(SessionData.empty)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("check-eori-details.title"),
          doc => {
            doc.select("form dl div dd").get(0).text() shouldBe eori.value
            doc.select("form dl div dd").get(1).text() shouldBe name.toFullName
          }
        )
      }

      "The user is logged in with a C2825 journey" in forAll { (eori: Eori, name: contactdetails.Name) =>
        val session = SessionData(OverpaymentsSingleJourney.empty(eori))

        inSequence {
          mockAuthRequiredRetrievals(eori, name)
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("check-eori-details.title"),
          doc => {
            doc.select("form dl div dd").get(0).text() shouldBe eori.value
            doc.select("form dl div dd").get(1).text() shouldBe name.toFullName
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
            doc.select("form dl div dd").get(1).text() shouldBe name.toFullName
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

        val eori        = sample[Eori]
        val contactName = ContactName("John Smith")

        inSequence {
          mockC285AuthRequiredRetrievals(eori, contactName)
          mockGetSession(SessionData.empty)
          mockGetEmail(Right(Some(CdsVerifiedEmail(verifiedEmail, ""))))
        }

        val result = performAction(Seq(checkEoriDetailsKey -> "true"))
        checkIsRedirect(result, commonRoutes.ChooseClaimTypeController.show())
      }

      "Redirect to the email frontend if user says details are correct but email address not verified" in {
        featureSwitch.disable(Feature.RejectedGoods)

        val eori        = sample[Eori]
        val contactName = ContactName("John Smith")

        inSequence {
          mockC285AuthRequiredRetrievals(eori, contactName)
          mockGetSession(SessionData.empty)
          mockGetEmail(Right(None))
        }

        val result = performAction(Seq(checkEoriDetailsKey -> "true"))
        checkIsRedirect(result, Call("GET", "http://localhost:9898/manage-email-cds/service/cds-reimbursement-claim"))
      }

      "Display an error page if user says details are correct but email address verification fails" in {
        featureSwitch.disable(Feature.RejectedGoods)

        val eori        = sample[Eori]
        val contactName = ContactName("John Smith")

        inSequence {
          mockC285AuthRequiredRetrievals(eori, contactName)
          mockGetSession(SessionData.empty)
          mockGetEmail(Left(Error("Cannot verify email address")))
        }

        val result = performAction(Seq(checkEoriDetailsKey -> "true"))
        checkIsTechnicalErrorPage(result)
      }

      "Redirect to signout if the user chooses the Eori is incorrect, logout option" in {
        val eori        = sample[Eori]
        val contactName = ContactName("John Smith")

        inSequence {
          mockC285AuthRequiredRetrievals(eori, contactName)
          mockGetSession(SessionData.empty)
        }

        val result = performAction(Seq(checkEoriDetailsKey -> "false"))
        checkIsRedirect(result, baseRoutes.StartController.start())
        session(result).data shouldBe Map.empty
      }

      "The user submits an invalid choice" in {
        val eori        = sample[Eori]
        val contactName = ContactName("John Smith")

        inSequence {
          mockC285AuthRequiredRetrievals(eori, contactName)
          mockGetSession(SessionData.empty)
        }

        val result = performAction(Seq())

        checkPageIsDisplayed(
          result,
          messageFromMessageKey("check-eori-details.title"),
          _.select("#check-eori-details-error")
            .html() shouldBe "<span class=\"govuk-visually-hidden\">Error:</span> " + messageFromMessageKey(
            "check-eori-details.error.invalid"
          ),
          BAD_REQUEST
        )
      }

      "The user submits no choice" in {
        val eori        = sample[Eori]
        val contactName = ContactName("John Smith")

        inSequence {
          mockC285AuthRequiredRetrievals(eori, contactName)
          mockGetSession(SessionData.empty)
        }

        val result = performAction(Seq.empty)

        checkPageIsDisplayed(
          result,
          messageFromMessageKey(s"$checkEoriDetailsKey.title"),
          _.select("#check-eori-details-error")
            .html() shouldBe "<span class=\"govuk-visually-hidden\">Error:</span> " + messageFromMessageKey(
            "check-eori-details.error.invalid"
          ),
          BAD_REQUEST
        )
      }
    }
  }
}
