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
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDetailsRegisteredWithCdsController.DetailsRegisteredWithCdsFormData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectNumberOfClaimsController.SelectNumberOfClaimsType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.DeclarantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.CompleteDeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DetailsRegisteredWithCdsAnswer.{CompleteDetailsRegisteredWithCdsAnswer, IncompleteDetailsRegisteredWithCdsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SelectNumberOfClaimsAnswer.CompleteSelectNumberOfClaimsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DetailsRegisteredWithCdsAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{sample, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

import scala.concurrent.Future

class EnterDetailsRegisteredWithCdsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: EnterDetailsRegisteredWithCdsController =
    instanceOf[EnterDetailsRegisteredWithCdsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  val fullName          = "enter-claimant-details-as-registered-with-cds.individual-full-name"
  val emailAddress      = "enter-claimant-details-as-registered-with-cds.individual-email"
  val addressLine1      = "nonUkAddress-line1"
  val addressLine2      = "nonUkAddress-line2"
  val addressLine3      = "nonUkAddress-line3"
  val addressLine4      = "nonUkAddress-line4"
  val postCode          = "postcode"
  val countryCode       = "countryCode"
  val addCompanyDetails = "enter-claimant-details-as-registered-with-cds.add-company-details"

  val goodData = Map(
    fullName          -> "Magnus Magnusson",
    emailAddress      -> "mangus@email.com",
    addressLine1      -> "57 Jex Belaran",
    addressLine2      -> "Eisraim Road",
    addressLine3      -> "",
    addressLine4      -> "Coventry",
    postCode          -> "CV3 6EA",
    countryCode       -> "GB",
    addCompanyDetails -> "false"
  )

  private def sessionWithClaimState(
    maybeClaimantDetailsAsIndividualAnswer: Option[DetailsRegisteredWithCdsAnswer],
    declarantType: Option[DeclarantType] = None
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(
        detailsRegisteredWithCdsAnswer = maybeClaimantDetailsAsIndividualAnswer,
        declarantTypeAnswer = declarantType.map(dt => CompleteDeclarantTypeAnswer(dt))
      )
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails =
      SignedInUserDetails(Some(email), eori, Email("email@email.com"), ContactName("Fred Bread"))
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
    def withDeclarantType(declarantType: DeclarantType): SessionData =
      sessionData.journeyStatus match {
        case Some(FillingOutClaim(g, s, (draftClaim: DraftC285Claim))) =>
          val answer   = Some(CompleteDeclarantTypeAnswer(declarantType))
          val newClaim = draftClaim.copy(declarantTypeAnswer = answer)
          sessionData.copy(journeyStatus = Some(FillingOutClaim(g, s, newClaim)))
        case _                                                         => fail("Failed to update DeclarantType")
      }
  }

  implicit class UpdateSessionWithAcc14Data(sessionData: SessionData) {
    def withAcc14Data(acc14Response: DisplayResponseDetail): SessionData =
      sessionData.journeyStatus match {
        case Some(FillingOutClaim(g, s, (draftClaim: DraftC285Claim))) =>
          val answer   = Some(DisplayDeclaration(acc14Response))
          val newClaim = draftClaim.copy(displayDeclaration = answer)
          sessionData.copy(journeyStatus = Some(FillingOutClaim(g, s, newClaim)))
        case _                                                         => fail("Failed to update DisplayResponseDetail")
      }

  }

  def getEstablishmentAddress(prefix: String): EstablishmentAddress =
    EstablishmentAddress(
      addressLine1 = s"$prefix.addLine1",
      addressLine2 = Some(s"$prefix.addLine2"),
      addressLine3 = Some(s"$prefix.addLine3"),
      postalCode = Some(s"$prefix.pc"),
      countryCode = "GB"
    )

  def getContactDetails(prefix: String): ContactDetails =
    ContactDetails(
      contactName = Some(s"$prefix.JohnSmith"),
      addressLine1 = Some(s"$prefix.addLine1"),
      addressLine2 = Some(s"$prefix.addLine2"),
      addressLine3 = Some(s"$prefix.addLine3"),
      addressLine4 = Some(s"$prefix.addLine4"),
      postalCode = Some(s"$prefix.postalCode"),
      countryCode = Some("GB"),
      telephone = Some(s"$prefix.telephone"),
      emailAddress = Some(s"$prefix.email")
    )

  "Enter Claimant Details As Individual controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        def performAction(): Future[Result] = controller.changeDetailsRegisteredWithCds()(FakeRequest())

        val answers = IncompleteDetailsRegisteredWithCdsAnswer.empty

        val (session, _, _) = sessionWithClaimState(Some(answers))

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
  }

  "enterDetailsRegisteredWithCds" must {

    def performAction() = controller.enterDetailsRegisteredWithCds()(FakeRequest())

    "Show data from Acc14.consigneeDetails.establishmentAddress if DeclarantType = Importer" in {
      val contactDetails       = getContactDetails("acc14.cons.cont")
      val establishmentAddress = getEstablishmentAddress("acc14.cons.est")
      val consignee            = sample[ConsigneeDetails]
        .copy(establishmentAddress = establishmentAddress, contactDetails = Some(contactDetails))
      val acc14Response        = sample[DisplayResponseDetail].copy(consigneeDetails = Some(consignee))

      val answers         = IncompleteDetailsRegisteredWithCdsAnswer.empty
      val (s, journey, _) = sessionWithClaimState(Some(answers))
      val session         = s
        .withDeclarantType(DeclarantType.Importer)
        .withAcc14Data(acc14Response)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val doc = Jsoup.parse(contentAsString(performAction()))

      doc
        .getElementById("enter-claimant-details-as-registered-with-cds.individual-full-name")
        .`val`()                                          shouldBe consignee.legalName
      doc
        .getElementById("enter-claimant-details-as-registered-with-cds.individual-email")
        .`val`()                                          shouldBe journey.signedInUserDetails.verifiedEmail.value
      doc.getElementById("nonUkAddress-line1").`val`()    shouldBe establishmentAddress.addressLine1
      doc.getElementById("nonUkAddress-line2").`val`()    shouldBe establishmentAddress.addressLine2.getOrElse(fail)
      doc.getElementById("nonUkAddress-line3").`val`()    shouldBe ""
      doc.getElementById("nonUkAddress-line4").`val`()    shouldBe establishmentAddress.addressLine3.getOrElse(fail)
      doc.getElementById("postcode").`val`()              shouldBe establishmentAddress.postalCode.getOrElse(fail)
      doc.select("#countryCode option[selected]").`val`() shouldBe establishmentAddress.countryCode
    }

    "Show data from Acc14.consigneeDetails.establishmentAddress if DeclarantType = AssociatedWithImporterCompany" in {
      val contactDetails       = getContactDetails("acc14.cons.cont")
      val establishmentAddress = getEstablishmentAddress("acc14.cons.est")
      val consignee            = sample[ConsigneeDetails]
        .copy(establishmentAddress = establishmentAddress, contactDetails = Some(contactDetails))
      val acc14Response        = sample[DisplayResponseDetail].copy(consigneeDetails = Some(consignee))

      val answers         = IncompleteDetailsRegisteredWithCdsAnswer.empty
      val (s, journey, _) = sessionWithClaimState(Some(answers))
      val session         = s
        .withDeclarantType(DeclarantType.AssociatedWithImporterCompany)
        .withAcc14Data(acc14Response)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val doc = Jsoup.parse(contentAsString(performAction()))

      doc
        .getElementById("enter-claimant-details-as-registered-with-cds.individual-full-name")
        .`val`()                                          shouldBe consignee.legalName
      doc
        .getElementById("enter-claimant-details-as-registered-with-cds.individual-email")
        .`val`()                                          shouldBe journey.signedInUserDetails.verifiedEmail.value
      doc.getElementById("nonUkAddress-line1").`val`()    shouldBe establishmentAddress.addressLine1
      doc.getElementById("nonUkAddress-line2").`val`()    shouldBe establishmentAddress.addressLine2.getOrElse(fail)
      doc.getElementById("nonUkAddress-line3").`val`()    shouldBe ""
      doc.getElementById("nonUkAddress-line4").`val`()    shouldBe establishmentAddress.addressLine3.getOrElse(fail)
      doc.getElementById("postcode").`val`()              shouldBe establishmentAddress.postalCode.getOrElse(fail)
      doc.select("#countryCode option[selected]").`val`() shouldBe establishmentAddress.countryCode
    }

    "Show data from Acc14.declarantDetails.establishmentAddress if DeclarantType = AssociatedWithRepresentativeCompany" in {
      val contactDetails       = getContactDetails("acc14.cons.cont")
      val establishmentAddress = getEstablishmentAddress("acc14.cons.est")
      val declarant            = sample[DeclarantDetails]
        .copy(establishmentAddress = establishmentAddress, contactDetails = Some(contactDetails))
      val acc14Response        = sample[DisplayResponseDetail].copy(declarantDetails = declarant)

      val answers         = IncompleteDetailsRegisteredWithCdsAnswer.empty
      val (s, journey, _) = sessionWithClaimState(Some(answers))
      val session         = s
        .withDeclarantType(DeclarantType.AssociatedWithRepresentativeCompany)
        .withAcc14Data(acc14Response)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val doc = Jsoup.parse(contentAsString(performAction()))

      doc
        .getElementById("enter-claimant-details-as-registered-with-cds.individual-full-name")
        .`val`()                                          shouldBe declarant.legalName
      doc
        .getElementById("enter-claimant-details-as-registered-with-cds.individual-email")
        .`val`()                                          shouldBe journey.signedInUserDetails.verifiedEmail.value
      doc.getElementById("nonUkAddress-line1").`val`()    shouldBe establishmentAddress.addressLine1
      doc.getElementById("nonUkAddress-line2").`val`()    shouldBe establishmentAddress.addressLine2.getOrElse(fail)
      doc.getElementById("nonUkAddress-line3").`val`()    shouldBe ""
      doc.getElementById("nonUkAddress-line4").`val`()    shouldBe establishmentAddress.addressLine3.getOrElse(fail)
      doc.getElementById("postcode").`val`()              shouldBe establishmentAddress.postalCode.getOrElse(fail)
      doc.select("#countryCode option[selected]").`val`() shouldBe establishmentAddress.countryCode
    }

  }

  "changeDetailsRegisteredWithCds" must {
    def performAction() = controller.changeDetailsRegisteredWithCds()(FakeRequest())

    "render previously completed data" in {
      val answers = CompleteDetailsRegisteredWithCdsAnswer(sample[DetailsRegisteredWithCdsFormData])
      val session = sessionWithClaimState(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val doc     = Jsoup.parse(contentAsString(performAction()))
      val address = answers.detailsRegisteredWithCds.contactAddress

      doc
        .getElementById("enter-claimant-details-as-registered-with-cds.individual-full-name")
        .`val`()                                       shouldBe answers.detailsRegisteredWithCds.fullName
      doc
        .getElementById("enter-claimant-details-as-registered-with-cds.individual-email")
        .`val`()                                       shouldBe answers.detailsRegisteredWithCds.emailAddress.value
      doc.getElementById("nonUkAddress-line1").`val`() shouldBe address.line1
      doc.getElementById("nonUkAddress-line2").`val`() shouldBe address.line2.getOrElse("")
      doc.getElementById("nonUkAddress-line3").`val`() shouldBe address.line3.getOrElse("")
      doc.getElementById("nonUkAddress-line4").`val`() shouldBe address.line4
      doc.getElementById("postcode").`val`()           shouldBe address.postcode
    }

    "render incomplete data" in {
      val answers = IncompleteDetailsRegisteredWithCdsAnswer(Some(sample[DetailsRegisteredWithCdsFormData]))
      val session = sessionWithClaimState(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val doc     = Jsoup.parse(contentAsString(performAction()))
      val address = answers.detailsRegisteredWithCds.getOrElse(fail).contactAddress

      doc
        .getElementById("enter-claimant-details-as-registered-with-cds.individual-full-name")
        .`val`()                                       shouldBe answers.detailsRegisteredWithCds.getOrElse(fail).fullName
      doc
        .getElementById("enter-claimant-details-as-registered-with-cds.individual-email")
        .`val`()                                       shouldBe answers.detailsRegisteredWithCds.getOrElse(fail).emailAddress.value
      doc.getElementById("nonUkAddress-line1").`val`() shouldBe address.line1
      doc.getElementById("nonUkAddress-line2").`val`() shouldBe address.line2.getOrElse("")
      doc.getElementById("nonUkAddress-line3").`val`() shouldBe address.line3.getOrElse("")
      doc.getElementById("nonUkAddress-line4").`val`() shouldBe address.line4
      doc.getElementById("postcode").`val`()           shouldBe address.postcode
    }

    "render empty data" in {
      val answers = IncompleteDetailsRegisteredWithCdsAnswer.empty
      val session = sessionWithClaimState(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val doc = Jsoup.parse(contentAsString(performAction()))

      doc.getElementById("enter-claimant-details-as-registered-with-cds.individual-full-name").`val`() shouldBe ""
      doc.getElementById("enter-claimant-details-as-registered-with-cds.individual-email").`val`()     shouldBe ""
      doc.getElementById("nonUkAddress-line1").`val`()                                                 shouldBe ""
      doc.getElementById("nonUkAddress-line2").`val`()                                                 shouldBe ""
      doc.getElementById("nonUkAddress-line3").`val`()                                                 shouldBe ""
      doc.getElementById("nonUkAddress-line4").`val`()                                                 shouldBe ""
      doc.getElementById("postcode").`val`()                                                           shouldBe ""
    }

  }

  "Submitting Details Registered with CDS" must {
    "Redirect according to the journey" in new TableDrivenPropertyChecks {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.enterDetailsRegisteredWithCdsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val testCases = Table(
        ("NumberOfClaimsType", "JourneyBindable"),
        (SelectNumberOfClaimsType.Individual, JourneyBindable.Single),
        (SelectNumberOfClaimsType.Bulk, JourneyBindable.Bulk),
        (SelectNumberOfClaimsType.Scheduled, JourneyBindable.Scheduled)
      )

      forAll(testCases) { (numberOfClaims, journeyBindable) =>
        val (session, fillingOutClaim, draftC285Claim) = sessionWithClaimState(None)

        val updatedJourney = fillingOutClaim.copy(draftClaim =
          draftC285Claim.copy(
            movementReferenceNumber = sampleMrnAnswer(),
            selectNumberOfClaimsAnswer = Some(CompleteSelectNumberOfClaimsAnswer(numberOfClaims))
          )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          mockStoreSession(Right(()))
        }
        checkIsRedirect(
          performAction(goodData.toSeq),
          routes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
        )
      }
    }

  }

  "Form Validation" must {
    val form = EnterDetailsRegisteredWithCdsController.detailsRegisteredWithCdsForm

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

    "Add Company Details" should {
      "accept true" in {
        val errors = form.bind(goodData.updated(addCompanyDetails, "true")).errors
        errors shouldBe Nil
      }
      "accept false" in {
        val errors = form.bind(goodData.updated(addCompanyDetails, "true")).errors
        errors shouldBe Nil
      }
      "accept reject anything else" in {
        val errors = form.bind(goodData.updated(addCompanyDetails, "truea")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.boolean")
      }
    }

  }
}
