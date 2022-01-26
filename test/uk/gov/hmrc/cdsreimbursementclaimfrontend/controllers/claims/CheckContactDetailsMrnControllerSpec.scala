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

import cats.Functor
import cats.Id
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckContactDetailsMrnController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AddressLookupSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.alphaCharGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyBindableGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PhoneNumberGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import java.net.URL
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckContactDetailsMrnControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AddressLookupSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[AddressLookupService].toInstance(addressLookupServiceMock)
    )

  private val featureSwitch = instanceOf[FeatureSwitchService]
  featureSwitch.enable(Feature.NorthernIreland)

  val controller: CheckContactDetailsMrnController = instanceOf[CheckContactDetailsMrnController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private def getSessionWithPreviousAnswer(
    displayDeclaration: Option[DisplayDeclaration],
    declarantTypeAnswer: Option[DeclarantTypeAnswer],
    maybeTypeOfClaim: Option[TypeOfClaimAnswer],
    mrnContactDetailsAnswer: Option[MrnContactDetails] = None,
    mrnContactAddressAnswer: Option[ContactAddress] = None
  ): (SessionData, FillingOutClaim) = {
    val draftC285Claim      = DraftClaim.blank.copy(
      displayDeclaration = displayDeclaration,
      declarantTypeAnswer = declarantTypeAnswer,
      typeOfClaim = maybeTypeOfClaim,
      movementReferenceNumber = Some(sample[MRN]),
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

    def showPageActionAddDetails(journey: JourneyBindable): Future[Result] =
      controller.addDetailsShow(journey)(FakeRequest())

    "redirect to the start of the journey" when {
      "there is no journey status in the session" in {
        val journey = sample[JourneyBindable]
        val session = getSessionWithPreviousAnswer(None, None, Some(toTypeOfClaim(journey)))._1

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
      "all mandatory data from Acc14 is available" in {
        val journey                    = sample[JourneyBindable]
        val acc14                      = genAcc14WithAddresses
        val mrnContactDetails          = sample[MrnContactDetails].copy(phoneNumber = Some(sample[PhoneNumber]))
        val mrnContactAddress          = sample[ContactAddress]
          .copy(line2 = Some(alphaCharGen(10)), line3 = Some(alphaCharGen(10)), country = Country.uk)
        val (session, fillingOutClaim) = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey)),
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

      "not all mandatory data from Acc14 is available, page redirects" in {
        val journey = sample[JourneyBindable]
        val acc14   = genAcc14WithAddresses

        val (session, _) = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey)),
          None,
          None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          showPageAction(journey),
          routes.CheckContactDetailsMrnController.addDetailsShow(journey)
        )

      }

      "not all mandatory data from Acc14 is available, no contact details are shown" in {
        val journey = sample[JourneyBindable]
        val acc14   = genAcc14WithAddresses

        val (session, fillingOutClaim) = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey)),
          None,
          None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          showPageActionAddDetails(journey),
          messageFromMessageKey("claimant-details.title"),
          doc => {
            val paragraphs = doc.select("dd > p")
            val consignee  = acc14.displayResponseDetail.consigneeDetails.getOrElse(fail())
            //Registered Details with CDS
            paragraphs.get(1).text() shouldBe consignee.contactDetails.flatMap(_.telephone).getOrElse(fail())
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
        controller.addDetailsSubmit(journeyBindable)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "user chooses the Yes option" in {
        val journey         = sample[JourneyBindable]
        val acc14           = genAcc14WithAddresses
        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey))
        )
        val fillingOutClaim = foc

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkIsRedirect(
          submitAdd(Seq(checkContactDetailsKey -> "true"), journey),
          routes.EnterContactDetailsMrnController.enterMrnContactDetails(journey)
        )
      }

      "user chooses the No option" in {
        featureSwitch.disable(Feature.NorthernIreland)
        val journey = sample[JourneyBindable]
        val acc14   = genAcc14WithAddresses

        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey))
        )
        val fillingOutClaim = foc

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkIsRedirect(
          submitAdd(Seq(checkContactDetailsKey -> "false"), journey),
          routes.SelectBasisForClaimController.selectBasisForClaim(journey)
        )
      }

      "the user does not select an option" in {
        val journey         = sample[JourneyBindable]
        val acc14           = genAcc14WithAddresses
        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey))
        )
        val fillingOutClaim = foc

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkPageIsDisplayed(
          submitAdd(Seq.empty, journey),
          messageFromMessageKey(s"$checkContactDetailsKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$checkContactDetailsKey.error.invalid"
            ),
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted" in {
        val journey         = sample[JourneyBindable]
        val acc14           = genAcc14WithAddresses
        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey))
        )
        val fillingOutClaim = foc

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkPageIsDisplayed(
          submitAdd(Seq(), journey),
          messageFromMessageKey(s"$checkContactDetailsKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$checkContactDetailsKey.error.invalid"
            ),
          BAD_REQUEST
        )
      }
    }

    "handle change submit requests" when {

      def submitChange(data: Seq[(String, String)], journeyBindable: JourneyBindable): Future[Result] =
        controller.submit(journeyBindable)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "user chooses the Yes option" in {
        featureSwitch.disable(Feature.NorthernIreland)
        val journey = sample[JourneyBindable]
        val acc14   = genAcc14WithAddresses
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey)),
          Some(sample[MrnContactDetails]),
          Some(sample[ContactAddress].copy(country = Country.uk))
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          submitChange(Seq(checkContactDetailsKey -> "true"), journey),
          routes.SelectBasisForClaimController.selectBasisForClaim(journey)
        )
      }

      "user chooses the No option" in {
        val acc14   = genAcc14WithAddresses
        val journey = sample[JourneyBindable]
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey)),
          Some(sample[MrnContactDetails]),
          Some(sample[ContactAddress].copy(country = Country.uk))
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          submitChange(Seq(checkContactDetailsKey -> "false"), journey),
          routes.CheckContactDetailsMrnController.addDetailsShow(journey)
        )
      }

      "the user does not select an option" in {
        val acc14   = genAcc14WithAddresses
        val journey = sample[JourneyBindable]
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey)),
          Some(sample[MrnContactDetails]),
          Some(sample[ContactAddress].copy(country = Country.uk))
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          submitChange(Seq.empty, journey),
          messageFromMessageKey(s"$checkContactDetailsKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$checkContactDetailsKey.error.invalid"
            ),
          BAD_REQUEST
        )
      }

      "an invalid option value is submitted" in {
        val acc14   = genAcc14WithAddresses
        val journey = sample[JourneyBindable]
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey)),
          Some(sample[MrnContactDetails]),
          Some(sample[ContactAddress].copy(country = Country.uk))
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          submitChange(Seq.empty, journey),
          messageFromMessageKey(s"$checkContactDetailsKey.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$checkContactDetailsKey.error.invalid"
            ),
          BAD_REQUEST
        )
      }
    }

    "Redirect to the problem page" when {
      def updateAddress(journey: JourneyBindable, maybeAddressId: Option[UUID]): Future[Result] =
        controller.retrieveAddressFromALF(journey, maybeAddressId)(FakeRequest())

      "user chooses an address without a post code" in {
        val id            = sample[UUID]
        val acc14         = genAcc14WithAddresses
        val journey       = sample[JourneyBindable]
        val (session, _)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey))
        )
        val errorResponse = Error("parsing address lookup response:/address/postcode: error.path.missing")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockAddressRetrieve(Left(errorResponse))
        }

        checkIsRedirect(
          updateAddress(journey, Some(id)),
          routes.ProblemWithAddressController.problem(journey)
        )
      }

      "user chooses an address without an address line 1" in {
        val journey       = sample[JourneyBindable]
        val id            = sample[UUID]
        val acc14         = genAcc14WithAddresses
        val (session, _)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toTypeOfClaim(journey))
        )
        val errorResponse = Error("parsing address lookup response:/address/lines: error.minLength")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockAddressRetrieve(Left(errorResponse))
        }

        checkIsRedirect(
          updateAddress(journey, Some(id)),
          routes.ProblemWithAddressController.problem(journey)
        )
      }
    }
  }

  "Validating session and acc14 data" should {

    "return true if we have valid session data" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(DeclarantTypeAnswer.Importer),
        Some(toTypeOfClaim(JourneyBindable.Single)),
        Some(sample[MrnContactDetails]),
        Some(sample[ContactAddress])
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe true
    }

    "return false if we have valid contact details but missing contact address" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(DeclarantTypeAnswer.Importer),
        Some(toTypeOfClaim(JourneyBindable.Single)),
        Some(sample[MrnContactDetails]),
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe false

    }
    "return false if we have valid contact address but missing contact details" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(DeclarantTypeAnswer.Importer),
        Some(toTypeOfClaim(JourneyBindable.Single)),
        None,
        Some(sample[ContactAddress])
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe false

    }

    "return true if we have no session data, but valid Acc14 data" in {
      val acc14           = genAcc14WithAddresses
      val fillingOutClaim = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toTypeOfClaim(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe false
    }

    "return false if we have no session data, and no contact name in Acc14 data" in {
      val fullAcc14        = genAcc14WithAddresses
      val consigneeDetails = Functor[Id].map(fullAcc14.displayResponseDetail.consigneeDetails.getOrElse(fail))(cd =>
        cd.copy(contactDetails = cd.contactDetails.map(contact => contact.copy(contactName = None)))
      )
      val acc14            = Functor[Id].map(fullAcc14)(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
      )
      val fillingOutClaim  = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toTypeOfClaim(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe false
    }

    "return false if we have no session data, and no contact address line1 in Acc14 data" in {
      val fullAcc14        = genAcc14WithAddresses
      val consigneeDetails = Functor[Id].map(fullAcc14.displayResponseDetail.consigneeDetails.getOrElse(fail))(cd =>
        cd.copy(contactDetails = cd.contactDetails.map(contact => contact.copy(addressLine1 = None)))
      )
      val acc14            = Functor[Id].map(fullAcc14)(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
      )
      val fillingOutClaim  = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toTypeOfClaim(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe false

    }

    "return false if we have no session data, and no contact address postcode in Acc14 data" in {
      val fullAcc14        = genAcc14WithAddresses
      val consigneeDetails = Functor[Id].map(fullAcc14.displayResponseDetail.consigneeDetails.getOrElse(fail))(cd =>
        cd.copy(contactDetails = cd.contactDetails.map(contact => contact.copy(postalCode = None)))
      )
      val acc14            = Functor[Id].map(fullAcc14)(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
      )
      val fillingOutClaim  = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toTypeOfClaim(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe false

    }
  }

  "The address lookup" should {

    "start successfully" in {
      val lookupUrl = sample[URL]
      val journey   = sample[JourneyBindable]

      inSequence {
        mockAuthWithNoRetrievals()
        mockAddressLookup(Right(lookupUrl))
      }

      checkIsRedirect(
        startAddressLookup(journey),
        lookupUrl.toString
      )
    }

    "fail to start if error response is received from downstream ALF service" in {
      val journey = sample[JourneyBindable]

      inSequence {
        mockAuthWithNoRetrievals()
        mockAddressLookup(Left(Error("Request was not accepted")))
      }

      checkIsTechnicalErrorPage(startAddressLookup(journey))
    }

    "update an address once complete" in {
      val id           = sample[UUID]
      val journey      = sample[JourneyBindable]
      val address      = sample[ContactAddress]
      val acc14        = genAcc14WithAddresses
      val (session, _) = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toTypeOfClaim(journey))
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Right(address))
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        retrieveLookupAddress(journey, Some(id)),
        routes.CheckContactDetailsMrnController.show(journey)
      )
    }

    "fail to update address once bad address lookup ID provided" in {
      val id           = sample[UUID]
      val acc14        = genAcc14WithAddresses
      val journey      = sample[JourneyBindable]
      val (session, _) = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toTypeOfClaim(journey))
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Left(Error(s"No address found for $id")))
      }

      checkIsTechnicalErrorPage(retrieveLookupAddress(journey, Some(id)))
    }

    "redirect to show page once address lookup ID is not provided" in {
      val journey      = sample[JourneyBindable]
      val acc14        = genAcc14WithAddresses
      val (session, _) = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(DeclarantTypeAnswer.Importer),
        Some(toTypeOfClaim(journey))
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkIsRedirect(
        retrieveLookupAddress(journey),
        routes.CheckContactDetailsMrnController.show(journey)
      )
    }

    def startAddressLookup(journey: JourneyBindable): Future[Result] =
      controller.redirectToALF(journey)(FakeRequest())

    def retrieveLookupAddress(journey: JourneyBindable, maybeAddressId: Option[UUID] = None): Future[Result] =
      controller.retrieveAddressFromALF(journey, maybeAddressId)(FakeRequest())
  }
}
