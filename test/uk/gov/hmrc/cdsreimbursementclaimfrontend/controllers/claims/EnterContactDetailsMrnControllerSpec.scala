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

import cats.implicits._
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyBindable, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.{Email, PhoneNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{sample, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PhoneNumberGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}

import scala.concurrent.Future

class EnterContactDetailsMrnControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with TableDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: EnterContactDetailsMrnController = instanceOf[EnterContactDetailsMrnController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  val signedInUserEmail = sample[Email]

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Multiple,
    JourneyBindable.Scheduled
  )

  val nameData  = "Mr Pip"
  val emailData = sample[Email]
  val phoneData = Some(sample[PhoneNumber])

  val goodData = Map(
    "enter-contact-details.contact-name"         -> "Mr Pip",
    "enter-contact-details.contact-email"        -> emailData.value,
    "enter-contact-details.contact-phone-number" -> phoneData.getOrElse(fail).value
  )

  private def getSessionWithPreviousAnswer(
    maybeMrnContactDetailsAnswer: Option[MrnContactDetails],
    mrnContactAddressAnswer: Option[ContactAddress],
    maybeTypeOfClaim: Option[TypeOfClaimAnswer]
  ): (SessionData, FillingOutClaim) = {
    val draftC285Claim      = DraftClaim.blank
      .copy(
        mrnContactDetailsAnswer = maybeMrnContactDetailsAnswer,
        mrnContactAddressAnswer = mrnContactAddressAnswer,
        typeOfClaim = maybeTypeOfClaim,
        movementReferenceNumber = Some(sample[MRN])
      )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (SessionData.empty.copyWith(journeyStatus = Some(journey)), journey)
  }

  private def updateSession(sessionData: SessionData, mrnContactDetailsAnswer: MrnContactDetails): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, draftClaim: DraftClaim)) =>
        val newClaim      =
          draftClaim.copy(mrnContactDetailsAnswer = Some(mrnContactDetailsAnswer))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copyWith(journeyStatus = Some(journeyStatus))
      case _                                                   => fail()
    }

  "Enter Or Change Contact Details controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.changeMrnContactDetails(journey)(FakeRequest())

        val (session, _) = getSessionWithPreviousAnswer(None, None, None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copyWith(journeyStatus = None))
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

        val session = getSessionWithPreviousAnswer(None, None, toTypeOfClaim(journey).some)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-contact-details.add.title")
        )
      }

      "the user has not answered this question before and is changing details" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.changeMrnContactDetails(journey)(FakeRequest())

        val contactDetailsAnswer = Some(sample[MrnContactDetails])
        val contactAddressAnswer = Some(sample[ContactAddress])
        val session              = getSessionWithPreviousAnswer(
          contactDetailsAnswer,
          contactAddressAnswer,
          toTypeOfClaim(journey).some
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-contact-details.change.title")
        )
      }

      "the user has entered contact details previously" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.changeMrnContactDetails(journey)(FakeRequest())
        val phone                           = Some(sample[PhoneNumber])
        val contactDetails                  = sample[MrnContactDetails].copy(phoneNumber = phone)
        val contactAddressAnswer            = Some(sample[ContactAddress])

        val answers = Some(contactDetails)
        val session =
          getSessionWithPreviousAnswer(answers, contactAddressAnswer, toTypeOfClaim(journey).some)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-contact-details.change.title"),
          doc => {
            doc
              .getElementById("enter-contact-details.contact-name")
              .`val`() shouldBe contactDetails.fullName
            doc
              .getElementById("enter-contact-details.contact-email")
              .`val`() shouldBe contactDetails.emailAddress.value
            doc
              .getElementById("enter-contact-details.contact-phone-number")
              .`val`() shouldBe contactDetails.phoneNumber.map(_.value).getOrElse(fail)
          }
        )
      }

    }

    "handle submit requests" when {

      def performAction(data: Seq[(String, String)], journey: JourneyBindable): Future[Result] =
        controller.changeMrnContactDetailsSubmit(journey)(FakeRequest().withFormUrlEncodedBody(data: _*))

      " the user enters details" in forAll(journeys) { journey =>
        val contactDetails = MrnContactDetails(nameData, emailData, phoneData)

        val session        = getSessionWithPreviousAnswer(None, None, toTypeOfClaim(journey).some)._1
        val updatedSession = updateSession(session, contactDetails)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(goodData.toSeq, journey),
          routes.CheckContactDetailsMrnController.show(journey)
        )
      }

      "the user did not enter any details" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(None, None, toTypeOfClaim(journey).some)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(Seq.empty, journey),
          messageFromMessageKey("enter-contact-details.change.title"),
          doc => {
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-contact-details.contact-name.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(2) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-contact-details.contact-email.error.required"
            )
          },
          BAD_REQUEST
        )
      }
    }

  }

  "Form Validation" must {
    val form         = EnterContactDetailsMrnController.mrnContactDetailsForm
    val fullName     = "enter-contact-details.contact-name"
    val emailAddress = "enter-contact-details.contact-email"
    val phone        = "enter-contact-details.contact-phone-number"

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
