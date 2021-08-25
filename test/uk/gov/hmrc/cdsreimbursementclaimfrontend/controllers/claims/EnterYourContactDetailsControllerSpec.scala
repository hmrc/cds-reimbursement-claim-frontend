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
import cats.{Functor, Id}
import org.jsoup.Jsoup
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{Country, Postcode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{sample, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

import scala.concurrent.Future

class EnterYourContactDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: EnterYourContactDetailsController =
    instanceOf[EnterYourContactDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  val signedInUserEmail = sample[Email]

  private def sessionWithClaimState(
    maybeContactDetails: Option[ContactDetailsAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(
        entryNumberContactDetailsAnswer = maybeContactDetails,
        movementReferenceNumber = sampleEntryNumberAnswer()
      )
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails =
      SignedInUserDetails(Some(email), eori, signedInUserEmail, ContactName("Fred Bread"))
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftC285Claim
    )
  }

  implicit class UpdateSessionWithDeclarantType(sessionData: SessionData) {
    def withDeclarantType(declarantType: DeclarantTypeAnswer): SessionData =
      sessionData.journeyStatus match {
        case Some(FillingOutClaim(g, s, (draftClaim: DraftC285Claim))) =>
          val answer   = Some(declarantType)
          val newClaim = draftClaim.copy(declarantTypeAnswer = answer)
          sessionData.copy(journeyStatus = Some(FillingOutClaim(g, s, newClaim)))
        case _                                                         => fail("Failed to update DeclarantType")
      }
  }

  "enterContactDetails" must {

    def performAction() = controller.enterContactDetails()(FakeRequest())

    "Show data previously saved data" in {
      val contactDetails = Functor[Id].map(sample[ContactDetailsAnswer])(a =>
        a.copy(
          contactAddress =
            a.contactAddress.copy(line2 = Some(alphaCharGen(10)), line3 = Some(alphaCharGen(10)), country = Country.uk)
        )
      )

      val session = sessionWithClaimState(Some(contactDetails))._1
        .withDeclarantType(DeclarantTypeAnswer.Importer)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val doc = Jsoup.parse(contentAsString(performAction()))

      doc.getElementById("enter-your-contact-details.contact-name").`val`()  shouldBe contactDetails.companyName
      doc
        .getElementById("enter-your-contact-details.contact-phone-number")
        .`val`()                                                             shouldBe contactDetails.phoneNumber.value
      doc.getElementById("enter-your-contact-details.contact-email").`val`() shouldBe contactDetails.emailAddress.value
      doc.getElementById("nonUkAddress-line1").`val`()                       shouldBe contactDetails.contactAddress.line1
      doc.getElementById("nonUkAddress-line2").`val`()                       shouldBe contactDetails.contactAddress.line2.getOrElse(fail)
      doc.getElementById("nonUkAddress-line3").`val`()                       shouldBe contactDetails.contactAddress.line3.getOrElse(fail)
      doc.getElementById("nonUkAddress-line4").`val`()                       shouldBe contactDetails.contactAddress.line4
      doc.getElementById("postcode").`val`()                                 shouldBe contactDetails.contactAddress.postcode
      doc.select("#countryCode option[selected]").`val`()                    shouldBe contactDetails.contactAddress.country.code
    }

    "Submitting Details Registered with CDS" must {

      val contactNameKey  = "enter-your-contact-details.contact-name"
      val emailAddressKey = "enter-your-contact-details.contact-email"
      val phoneNumberKey  = "enter-your-contact-details.contact-phone-number"
      val addressLine1Key = "nonUkAddress-line1"
      val addressLine2Key = "nonUkAddress-line2"
      val addressLine3Key = "nonUkAddress-line3"
      val addressLine4Key = "nonUkAddress-line4"
      val postCodeKey     = "postcode"
      val countryCodeKey  = "countryCode"

      val goodData = Map(
        contactNameKey  -> "Magnus Magnusson",
        emailAddressKey -> "mangus@email.com",
        phoneNumberKey  -> "07575757575",
        addressLine1Key -> "57 Jex Belaran",
        addressLine2Key -> "Eisraim Road",
        addressLine3Key -> "",
        addressLine4Key -> "Coventry",
        postCodeKey     -> "CV3 6EA",
        countryCodeKey  -> "GB"
      )

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.enterContactDetailsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "Redirect according to the journey" in {
        val (session, fillingOutClaim, draftC285Claim) = sessionWithClaimState(None)

        val updatedJourney = fillingOutClaim.copy(draftClaim =
          draftC285Claim.copy(
            movementReferenceNumber = sampleMrnAnswer(),
            selectNumberOfClaimsAnswer = Some(SelectNumberOfClaimsAnswer.Individual),
            declarantTypeAnswer = Some(DeclarantTypeAnswer.AssociatedWithImporterCompany)
          )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          mockStoreSession(Right(()))
        }
        checkIsRedirect(
          performAction(goodData.toSeq),
          routes.SelectBasisForClaimController.selectBasisForClaim(JourneyBindable.Single)
        )
      }
    }

  }

  "Form Validation" must {
    val form         = EnterYourContactDetailsController.contactDetailsForm
    val fullName     = "enter-your-contact-details.contact-name"
    val emailAddress = "enter-your-contact-details.contact-email"
    val phone        = "enter-your-contact-details.contact-phone-number"
    val addressLine1 = "nonUkAddress-line1"
    val addressLine2 = "nonUkAddress-line2"
    val addressLine3 = "nonUkAddress-line3"
    val addressLine4 = "nonUkAddress-line4"
    val postCode     = "postcode"
    val countryCode  = "countryCode"

    val goodData = Map(
      fullName     -> "Nephilim Import Export Ltd",
      emailAddress -> "info@nephilim.at",
      phone        -> "0155555555",
      addressLine1 -> "57 Plateau of Sorana",
      addressLine2 -> "Lasseera",
      addressLine3 -> "",
      addressLine4 -> "Coventry",
      postCode     -> "CV3 6EA",
      countryCode  -> "GB"
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
        val email  = List.fill(233)("a").mkString("") + "@abc.com" //Allthogether 124
        val errors = form.bind(goodData.updated(emailAddress, email)).errors
        errors shouldBe Nil
      }
      "Reject email too long" in {
        val email  = List.fill(234)("a").mkString("") + "@abc.com" //Allthogether 125
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

    "Address Line 1" should {
      "Accept longest possible address line" in {
        val errors = form.bind(goodData.updated(addressLine1, alphaNumGen(35))).errors
        errors shouldBe Nil
      }
      "Reject address lines too long" in {
        val errors = form.bind(goodData.updated(addressLine1, alphaNumGen(36))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooLong")
      }
    }

    "Address Line 2" should {
      "Accept longest possible address line" in {
        val errors = form.bind(goodData.updated(addressLine2, alphaNumGen(35))).errors
        errors shouldBe Nil
      }
      "Reject address lines too long" in {
        val errors = form.bind(goodData.updated(addressLine2, alphaNumGen(36))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooLong")
      }
    }

    "Address Line 3" should {
      "Accept longest possible address line" in {
        val errors = form.bind(goodData.updated(addressLine3, alphaNumGen(35))).errors
        errors shouldBe Nil
      }
      "Reject address lines too long" in {
        val errors = form.bind(goodData.updated(addressLine3, alphaNumGen(36))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooLong")
      }
    }

    "Address Line 4" should {
      "Accept longest possible address line" in {
        val errors = form.bind(goodData.updated(addressLine4, alphaNumGen(35))).errors
        errors shouldBe Nil
      }
      "Reject address lines too long" in {
        val errors = form.bind(goodData.updated(addressLine4, alphaNumGen(36))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooLong")
      }
    }

    "Postcode" should {
      "Accept longest possible post code" in {
        val errors = form.bind(goodData.updated(postCode, "BD17 7DG")).errors
        errors shouldBe Nil
      }
      "Reject post code too long after trim" in {
        val errors = form.bind(goodData.updated(postCode, "BDD17 7DG")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooLong")
      }
      "Reject invalid post code" in {
        val errors = form.bind(goodData.updated(postCode, "BDD7 7DG")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.pattern")
      }
      "Reject post code if too long" in {
        val errors = form.bind(goodData.updated(postCode, "BD17 7DGGA")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
      "Accept Anything outside of the UK" in {
        val errors = form.bind(goodData.updated(countryCode, "IS").updated(postCode, "AA2")).errors
        errors shouldBe Nil
      }
      "Reject Anything outside of the UK if it's too long" in {
        val errors = form.bind(goodData.updated(countryCode, "IS").updated(postCode, "1234567890")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }

      "Unbind should work" in {
        val postcode = "BD17 7DG"
        Postcode.mapping.unbind(postcode).values.toSeq shouldBe Seq(postcode)
      }
    }

    "Country Code" must {
      "Accept 2 digits country code" in {
        val errors = form.bind(goodData.updated(countryCode, "HU")).errors
        errors shouldBe Nil
      }

      "Reject 1 digit" in {
        val errors = form.bind(goodData.updated(countryCode, "H")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.notFound")
      }

      "Reject 3 digits" in {
        val errors = form.bind(goodData.updated(countryCode, "HUH")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.notFound")
      }
    }

  }
}
