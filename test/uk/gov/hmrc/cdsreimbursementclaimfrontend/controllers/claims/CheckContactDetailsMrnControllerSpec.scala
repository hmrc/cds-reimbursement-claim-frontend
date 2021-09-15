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
import org.scalatest.BeforeAndAfterEach
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckContactDetailsMrnController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AddressLookupSupport, AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{ContactAddress, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{alphaCharGen, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PhoneNumberGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarantTypeAnswer, Error, MovementReferenceNumber, MrnContactDetails, SelectNumberOfClaimsAnswer, SessionData, SignedInUserDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{AddressLookupService, FeatureSwitchService}

import java.net.URL
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckContactDetailsMrnControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AddressLookupSupport
    with BeforeAndAfterEach
    with TableDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[AddressLookupService].toInstance(addressLookupServiceMock)
    )

  val featureSwitch = instanceOf[FeatureSwitchService]
  featureSwitch.NorthernIreland.enable()

  val controller: CheckContactDetailsMrnController = instanceOf[CheckContactDetailsMrnController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Multiple,
    JourneyBindable.Scheduled
  )

  private def getSessionWithPreviousAnswer(
    displayDeclaration: Option[DisplayDeclaration],
    declarantTypeAnswer: Option[DeclarantTypeAnswer],
    selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer],
    mrnContactDetailsAnswer: Option[MrnContactDetails] = None,
    mrnContactAddressAnswer: Option[ContactAddress] = None
  ): (SessionData, FillingOutClaim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(
      displayDeclaration = displayDeclaration,
      declarantTypeAnswer = declarantTypeAnswer,
      selectNumberOfClaimsAnswer = selectNumberOfClaimsAnswer,
      movementReferenceNumber = Some(sample[MovementReferenceNumber]),
      mrnContactDetailsAnswer = mrnContactDetailsAnswer,
      mrnContactAddressAnswer = mrnContactAddressAnswer
    )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey
    )
  }

  "CheckContactDetailsMrnController" must {

    def showPageAction(journey: JourneyBindable): Future[Result] =
      controller.show(journey)(FakeRequest())

    "redirect to the start of the journey" when {
      "there is no journey status in the session" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(None, None, Some(toSelectNumberOfClaims(journey)))._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          showPageAction(journey),
          baseRoutes.StartController.start()
        )
      }
    }

    "display the page" when {
      "all mandatory data from Acc14 is available" in forAll(journeys) { journey =>
        val acc14                      = generateAcc14WithAddresses()
        val mrnContactDetails          = sample[MrnContactDetails].copy(phoneNumber = Some(sample[PhoneNumber]))
        val mrnContactAddress          = sample[ContactAddress]
          .copy(line2 = Some(alphaCharGen(10)), line3 = Some(alphaCharGen(10)), country = Country.uk)
        val (session, fillingOutClaim) = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey)),
          Some(mrnContactDetails),
          Some(mrnContactAddress)
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          showPageAction(journey),
          messageFromMessageKey("claimant-details.title"),
          doc => {
            val paragraphs = doc.select("dd > p")
            val consignee  = acc14.displayResponseDetail.consigneeDetails.getOrElse(fail())
            //Registered Details with CDS
            paragraphs.get(0).text()  shouldBe consignee.legalName
            paragraphs.get(1).text()  shouldBe consignee.contactDetails.flatMap(_.telephone).getOrElse(fail())
            paragraphs.get(2).text()  shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail.value
            paragraphs.get(3).text()  shouldBe consignee.establishmentAddress.addressLine1
            paragraphs.get(4).text()  shouldBe consignee.establishmentAddress.addressLine2.getOrElse(fail)
            paragraphs.get(5).text()  shouldBe consignee.establishmentAddress.addressLine3.getOrElse(fail)
            paragraphs.get(6).text()  shouldBe consignee.establishmentAddress.postalCode.getOrElse(fail)
            //Contact Details
            paragraphs.get(7).text()  shouldBe mrnContactDetails.fullName
            paragraphs.get(8).text()  shouldBe mrnContactDetails.emailAddress.value
            paragraphs.get(9).text()  shouldBe mrnContactDetails.phoneNumber.map(_.value).getOrElse(fail)
            paragraphs.get(10).text() shouldBe mrnContactAddress.line1
            paragraphs.get(11).text() shouldBe mrnContactAddress.line2.getOrElse(fail)
            paragraphs.get(12).text() shouldBe mrnContactAddress.line3.getOrElse(fail)
            paragraphs.get(13).text() shouldBe mrnContactAddress.line4
            paragraphs.get(14).text() shouldBe mrnContactAddress.postcode
            paragraphs.get(15).text() shouldBe "United Kingdom"
            paragraphs.size()         shouldBe 16
          }
        )
      }

      "not all mandatory data from Acc14 is available, no contact address is shown" in forAll(journeys) { journey =>
        val acc14 = generateAcc14WithAddresses()

        val (session, fillingOutClaim) = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey)),
          None,
          None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkPageIsDisplayed(
          showPageAction(journey),
          messageFromMessageKey("claimant-details.title"),
          doc => {
            val paragraphs = doc.select("dd > p")
            val consignee  = acc14.displayResponseDetail.consigneeDetails.getOrElse(fail())
            //Registered Details with CDS
            paragraphs.get(0).text() shouldBe consignee.legalName
            paragraphs.get(1).text() shouldBe consignee.contactDetails.flatMap(_.telephone).getOrElse(fail)
            paragraphs.get(2).text() shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail.value
            paragraphs.get(3).text() shouldBe consignee.establishmentAddress.addressLine1
            paragraphs.get(4).text() shouldBe consignee.establishmentAddress.addressLine2.getOrElse(fail)
            paragraphs.get(5).text() shouldBe consignee.establishmentAddress.addressLine3.getOrElse(fail)
            paragraphs.get(6).text() shouldBe consignee.establishmentAddress.postalCode.getOrElse(fail)
            paragraphs.size()        shouldBe 7
          }
        )
      }
    }

    "handle add submit requests" when {

      def submitAdd(data: Seq[(String, String)], journeyBindable: JourneyBindable): Future[Result] =
        controller.addDetails(journeyBindable)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "user chooses the Yes option" in forAll(journeys) { journey =>
        val acc14           = generateAcc14WithAddresses()
        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey))
        )
        val fillingOutClaim = foc

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkIsRedirect(
          submitAdd(Seq(languageKey -> "0"), journey),
          routes.EnterContactDetailsMrnController.enterMrnContactDetails(journey)
        )
      }

      "user chooses the No option" in forAll(journeys) { journey =>
        featureSwitch.NorthernIreland.disable()
        val acc14           = generateAcc14WithAddresses()
        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey))
        )
        val fillingOutClaim = foc

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkIsRedirect(
          submitAdd(Seq(languageKey -> "1"), journey),
          routes.SelectBasisForClaimController.selectBasisForClaim(journey)
        )
      }
      "the user does not select an option" in forAll(journeys) { journey =>
        val acc14           = generateAcc14WithAddresses()
        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey))
        )
        val fillingOutClaim = foc

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkPageIsDisplayed(
          submitAdd(Seq.empty, journey),
          messageFromMessageKey(s"$languageKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$languageKey.error.required.add"
            ),
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted" in forAll(journeys) { journey =>
        forAll(Table("Invalid Answers", "2", "3")) { invalidAnswer =>
          val acc14           = generateAcc14WithAddresses()
          val (session, foc)  = getSessionWithPreviousAnswer(
            Some(acc14),
            Some(DeclarantTypeAnswer.Importer),
            Some(toSelectNumberOfClaims(journey))
          )
          val fillingOutClaim = foc

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
          }

          checkPageIsDisplayed(
            submitAdd(Seq(languageKey -> invalidAnswer), journey),
            messageFromMessageKey(s"$languageKey.title"),
            doc =>
              doc
                .select(".govuk-error-summary__list > li > a")
                .text() shouldBe messageFromMessageKey(
                s"$languageKey.invalid"
              ),
            BAD_REQUEST
          )
        }
      }

    }

    "handle change submit requests" when {

      def submitChange(data: Seq[(String, String)], journeyBindable: JourneyBindable): Future[Result] =
        controller.submit(journeyBindable)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "user chooses the Yes option" in forAll(journeys) { journey =>
        featureSwitch.NorthernIreland.disable()
        val acc14   = generateAcc14WithAddresses()
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey)),
          Some(sample[MrnContactDetails]),
          Some(sample[ContactAddress].copy(country = Country.uk))
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          submitChange(Seq(languageKey -> "0"), journey),
          routes.SelectBasisForClaimController.selectBasisForClaim(journey)
        )
      }

      "user chooses the No option" in forAll(journeys) { journey =>
        val acc14   = generateAcc14WithAddresses()
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey)),
          Some(sample[MrnContactDetails]),
          Some(sample[ContactAddress].copy(country = Country.uk))
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          submitChange(Seq(languageKey -> "1"), journey),
          routes.CheckContactDetailsMrnController.show(journey)
        )
      }
      "the user does not select an option" in forAll(journeys) { journey =>
        val acc14   = generateAcc14WithAddresses()
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey)),
          Some(sample[MrnContactDetails]),
          Some(sample[ContactAddress].copy(country = Country.uk))
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          submitChange(Seq.empty, journey),
          messageFromMessageKey(s"$languageKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$languageKey.error.required.change"
            ),
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted" in forAll(journeys) { journey =>
        forAll(Table("Invalid Answers", "2", "3")) { invalidAnswer =>
          val acc14   = generateAcc14WithAddresses()
          val session = getSessionWithPreviousAnswer(
            Some(acc14),
            Some(DeclarantTypeAnswer.Importer),
            Some(toSelectNumberOfClaims(journey)),
            Some(sample[MrnContactDetails]),
            Some(sample[ContactAddress].copy(country = Country.uk))
          )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            submitChange(Seq(languageKey -> invalidAnswer), journey),
            messageFromMessageKey(s"$languageKey.title"),
            doc =>
              doc
                .select(".govuk-error-summary__list > li > a")
                .text() shouldBe messageFromMessageKey(
                s"$languageKey.invalid"
              ),
            BAD_REQUEST
          )
        }
      }

    }

  }

  "CheckContactDetailsMrnController Companion object" should {

    "Acc14 extractors for DeclarantTypeAnswer.Importer" in {
      val acc14           = generateAcc14WithAddresses()
      val acc14consignee  = acc14.displayResponseDetail.consigneeDetails
      val fillingOutClaim = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single))
      )._2

      val namePhoneEmail = extractDetailsRegisteredWithCDS(fillingOutClaim)
      namePhoneEmail.name                     shouldBe acc14consignee.map(_.legalName)
      namePhoneEmail.phoneNumber.map(_.value) shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail

      val address = extractEstablishmentAddress(fillingOutClaim)
      address shouldBe acc14consignee.map(_.establishmentAddress)

    }

    "Acc14 extractors for DeclarantTypeAnswer.AssociatedWithImporterCompany" in {
      val acc14           = generateAcc14WithAddresses()
      val acc14consignee  = acc14.displayResponseDetail.consigneeDetails
      val fillingOutClaim =
        getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.AssociatedWithImporterCompany),
          Some(toSelectNumberOfClaims(JourneyBindable.Single))
        )._2

      val namePhoneEmail = extractDetailsRegisteredWithCDS(fillingOutClaim)
      namePhoneEmail.name                     shouldBe acc14consignee.map(_.legalName)
      namePhoneEmail.phoneNumber.map(_.value) shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail

      val address = extractEstablishmentAddress(fillingOutClaim)
      address shouldBe acc14consignee.map(_.establishmentAddress)

    }

    "Acc14 extractors for DeclarantTypeAnswer.AssociatedWithRepresentativeCompany" in {
      val acc14           = generateAcc14WithAddresses()
      val acc14Declarant  = acc14.displayResponseDetail.declarantDetails
      val fillingOutClaim =
        getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.AssociatedWithRepresentativeCompany),
          Some(toSelectNumberOfClaims(JourneyBindable.Single))
        )._2

      val namePhoneEmail = extractDetailsRegisteredWithCDS(fillingOutClaim)
      namePhoneEmail.name.getOrElse(fail())   shouldBe acc14Declarant.legalName
      namePhoneEmail.phoneNumber.map(_.value) shouldBe acc14Declarant.contactDetails.flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail

      val address = extractEstablishmentAddress(fillingOutClaim)
      address.getOrElse(fail) shouldBe acc14Declarant.establishmentAddress

    }

  }

  "Validating session and acc14 data" should {

    "return true if we have valid session data" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single)),
        Some(sample[MrnContactDetails]),
        Some(sample[ContactAddress])
      )._2

      fillingOutClaim.draftClaim.isMandatoryDataAvailable shouldBe true
    }

    "return false if we have valid contact details but missing contact address" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single)),
        Some(sample[MrnContactDetails]),
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryDataAvailable shouldBe false

    }
    "return false if we have valid contact address but missing contact details" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single)),
        None,
        Some(sample[ContactAddress])
      )._2

      fillingOutClaim.draftClaim.isMandatoryDataAvailable shouldBe false

    }

    "return true if we have no session data, but valid Acc14 data" in {
      val acc14           = generateAcc14WithAddresses()
      val fillingOutClaim = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryDataAvailable shouldBe false
    }

    "return false if we have no session data, and no contact name in Acc14 data" in {
      val fullAcc14        = generateAcc14WithAddresses()
      val consigneeDetails = Functor[Id].map(fullAcc14.displayResponseDetail.consigneeDetails.getOrElse(fail))(cd =>
        cd.copy(contactDetails = cd.contactDetails.map(contact => contact.copy(contactName = None)))
      )
      val acc14            = Functor[Id].map(fullAcc14)(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
      )
      val fillingOutClaim  = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryDataAvailable shouldBe false
    }

    "return false if we have no session data, and no contact address line1 in Acc14 data" in {
      val fullAcc14        = generateAcc14WithAddresses()
      val consigneeDetails = Functor[Id].map(fullAcc14.displayResponseDetail.consigneeDetails.getOrElse(fail))(cd =>
        cd.copy(contactDetails = cd.contactDetails.map(contact => contact.copy(addressLine1 = None)))
      )
      val acc14            = Functor[Id].map(fullAcc14)(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
      )
      val fillingOutClaim  = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryDataAvailable shouldBe false

    }

    "return false if we have no session data, and no contact address postcode in Acc14 data" in {
      val fullAcc14        = generateAcc14WithAddresses()
      val consigneeDetails = Functor[Id].map(fullAcc14.displayResponseDetail.consigneeDetails.getOrElse(fail))(cd =>
        cd.copy(contactDetails = cd.contactDetails.map(contact => contact.copy(postalCode = None)))
      )
      val acc14            = Functor[Id].map(fullAcc14)(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
      )
      val fillingOutClaim  = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryDataAvailable shouldBe false

    }
  }

  "The address lookup" should {

    "start successfully" in forAll(journeys) { journey =>
      val lookupUrl = sample[URL]
      val session   = getSessionWithPreviousAnswer(None, None, Some(toSelectNumberOfClaims(journey)))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
        mockAddressLookupInitiation(Right(lookupUrl))
      }

      checkIsRedirect(
        changeAddress(journey),
        lookupUrl.toString
      )
    }

    "fail to start once error response received downstream ALF service" in forAll(journeys) { journey =>
      val session = getSessionWithPreviousAnswer(None, None, Some(toSelectNumberOfClaims(journey)))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
        mockAddressLookupInitiation(Left(Error("Request was not accepted")))
      }

      checkIsTechnicalErrorPage(changeAddress(journey))
    }

    "update an address once complete" in forAll(journeys) { journey =>
      val id           = sample[UUID]
      val address      = sample[ContactAddress]
      val acc14        = generateAcc14WithAddresses()
      val (session, _) = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(journey))
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Right(address))
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        updateAddress(journey, Some(id)),
        routes.CheckContactDetailsMrnController.show(journey)
      )
    }

    "fail to update address once bad address lookup ID provided" in forAll(journeys) { journey =>
      val id           = sample[UUID]
      val acc14        = generateAcc14WithAddresses()
      val (session, _) = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(journey))
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Left(Error(s"No address found for $id")))
      }

      checkIsTechnicalErrorPage(updateAddress(journey, Some(id)))
    }

    "redirect to show page once address lookup ID is not provided" in forAll(journeys) { journey =>
      val acc14        = generateAcc14WithAddresses()
      val (session, _) = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(journey))
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkIsRedirect(
        updateAddress(journey),
        routes.CheckContactDetailsMrnController.show(journey)
      )
    }

    def changeAddress(journey: JourneyBindable): Future[Result] =
      controller.changeAddress(journey)(FakeRequest())

    def updateAddress(journey: JourneyBindable, maybeAddressId: Option[UUID] = None): Future[Result] =
      controller.updateAddress(journey, maybeAddressId)(FakeRequest())
  }
}
