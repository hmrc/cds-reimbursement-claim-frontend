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

import org.jsoup.Jsoup
import org.scalatest.prop.TableDrivenPropertyChecks
import cats.implicits._
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Action, AnyContent, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{BAD_REQUEST, contentAsString, defaultAwaitTimeout}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PhoneNumberGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.MrnContactDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{sample, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

import scala.concurrent.Future

class EnterOrChangeContactDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with TableDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: EnterOrChangeContactDetailsController = instanceOf[EnterOrChangeContactDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  val signedInUserEmail = sample[Email]

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Bulk,
    JourneyBindable.Scheduled
  )

  val nameData  = "Mr Pip"
  val emailData = sample[Email]
  val phoneData = sample[PhoneNumber]

  val goodData = Map(
    "enter-or-change-contact-details.contact-name"         -> "Mr Pip",
    "enter-or-change-contact-details.contact-email"        -> emailData.value,
    "enter-or-change-contact-details.contact-phone-number" -> phoneData.value
  )

  private def getSessionWithPreviousAnswer(
    maybeMrnContactDetailsAnswer: Option[MrnContactDetails],
    selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer]
  ): (SessionData, FillingOutClaim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim
      .copy(
        mrnContactDetailsAnswer = maybeMrnContactDetailsAnswer,
        selectNumberOfClaimsAnswer = selectNumberOfClaimsAnswer,
        movementReferenceNumber = Some(sample[MovementReferenceNumber])
      )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (SessionData.empty.copy(journeyStatus = Some(journey)), journey)
  }

  private def updateSession(sessionData: SessionData, mrnContactDetailsAnswer: MrnContactDetails): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, (draftClaim: DraftC285Claim))) =>
        val newClaim      =
          draftClaim.copy(mrnContactDetailsAnswer = Some(mrnContactDetailsAnswer))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                         => fail()
    }

  def performAction(data: Seq[(String, String)] = Seq.empty)(action: Action[AnyContent]): Future[Result] =
    action()(FakeRequest().withFormUrlEncodedBody(data: _*))

  "Enter Or Change Contact Details controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.changeMrnContactDetails(journey)(FakeRequest())

        val (session, _) = getSessionWithPreviousAnswer(None, None)

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

      "the user has not answered this question before and is adding details" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.enterMrnContactDetails(journey)(FakeRequest())

        val session = getSessionWithPreviousAnswer(None, toSelectNumberOfClaims(journey).some)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-or-change-contact-details.add.title")
        )
      }

      "the user has not answered this question before and is changing details" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.changeMrnContactDetails(journey)(FakeRequest())

        val session = getSessionWithPreviousAnswer(None, toSelectNumberOfClaims(journey).some)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-or-change-contact-details.change.title")
        )
      }
    }

    "handle submit requests" when {

      " the user enters details" in forAll(journeys) { journey =>
        val contactDetails = MrnContactDetails(nameData, emailData, phoneData)

        val session        = getSessionWithPreviousAnswer(None, toSelectNumberOfClaims(journey).some)._1
        val updatedSession = updateSession(session, contactDetails)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(goodData.toSeq)(controller.changeMrnContactDetailsSubmit(journey)),
          routes.CheckClaimantDetailsController.show(journey)
        )
      }

    }

    "show an error summary" when {

      "the user does not enter any details" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(None, toSelectNumberOfClaims(journey).some)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction()(controller.enterMrnContactDetailsSubmit(journey)),
          messageFromMessageKey("enter-or-change-contact-details.add.title"),
          doc => {
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-or-change-contact-details.contact-name.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(2) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-or-change-contact-details.contact-email.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(3) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-or-change-contact-details.contact-phone-number.error.required"
            )
          },
          BAD_REQUEST
        )
      }
    }

    "show data" when {

      "the user has entered contact details" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.enterMrnContactDetails(journey)(FakeRequest())
        val name                            = "Mr Pip"
        val email                           = sample[Email]
        val phone                           = sample[PhoneNumber]

        val contactDetails = sample[MrnContactDetails].copy(fullName = name, emailAddress = email, phoneNumber = phone)

        val answers = Some(contactDetails)
        val session = getSessionWithPreviousAnswer(answers, toSelectNumberOfClaims(journey).some)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val doc = Jsoup.parse(contentAsString(performAction()))

        doc
          .getElementById("enter-or-change-contact-details.contact-name")
          .`val`() shouldBe contactDetails.fullName
        doc
          .getElementById("enter-or-change-contact-details.contact-email")
          .`val`() shouldBe contactDetails.emailAddress.value
        doc
          .getElementById("enter-or-change-contact-details.contact-phone-number")
          .`val`() shouldBe contactDetails.phoneNumber.value

      }
    }

  }

  "Form Validation" must {
    val form         = EnterOrChangeContactDetailsController.mrnContactDetailsForm
    val fullName     = "enter-or-change-contact-details.contact-name"
    val emailAddress = "enter-or-change-contact-details.contact-email"
    val phone        = "enter-or-change-contact-details.contact-phone-number"

    val goodData = Map(
      fullName     -> "Nephilim Import Export Ltd",
      emailAddress -> "info@nephilim.at",
      phone        -> "0155555555"
    )

    "accept good declaration details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "Name" should {
      "Accept longest possible names" in {
        val errors = form.bind(goodData.updated(fullName, alphaNumGen(512))).errors
        errors shouldBe Nil
      }
      "Reject names too long" in {
        val errors = form.bind(goodData.updated(fullName, alphaNumGen(513))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Email" should {
      "Accept longest possible email" in {
        val email  = List.fill(233)("a").mkString("") + "@abc.com"
        val errors = form.bind(goodData.updated(emailAddress, email)).errors
        errors shouldBe Nil
      }
      "Reject email too long" in {
        val email  = List.fill(234)("a").mkString("") + "@abc.com"
        val errors = form.bind(goodData.updated(emailAddress, email)).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

    "Phone" should {
      "Accept longest possible number" in {
        val errors = form.bind(goodData.updated(phone, numStringGen(30))).errors
        errors shouldBe Nil
      }
      "Reject numbers too long" in {
        val errors = form.bind(goodData.updated(phone, numStringGen(31))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }

  }
}
