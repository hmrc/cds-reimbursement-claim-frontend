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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckClaimantDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarantTypeAnswer, MovementReferenceNumber, MrnContactDetails, SelectNumberOfClaimsAnswer, SessionData, SignedInUserDetails}

import scala.concurrent.Future

class CheckClaimantDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TableDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: CheckClaimantDetailsController = instanceOf[CheckClaimantDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Bulk,
    JourneyBindable.Scheduled
  )

  private def getSessionWithPreviousAnswer(
    displayDeclaration: Option[DisplayDeclaration],
    declarantTypeAnswer: Option[DeclarantTypeAnswer],
    selectNumberOfClaimsAnswer: Option[SelectNumberOfClaimsAnswer],
    mrnContactDetailsAnswer: Option[MrnContactDetails] = None,
    mrnContactAddressAnswer: Option[NonUkAddress] = None
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

  "CheckClaimantDetailsController" must {

    def showPageAction(journey: JourneyBindable): Future[Result] = controller.show(journey)(FakeRequest())

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
        val (session, fillingOutClaim) = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey))
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
            paragraphs.get(7).text()  shouldBe consignee.contactDetails.flatMap(_.contactName).getOrElse(fail)
            paragraphs.get(8).text()  shouldBe consignee.contactDetails.flatMap(_.telephone).getOrElse(fail)
            paragraphs.get(9).text()  shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail.value
            paragraphs.get(10).text() shouldBe consignee.contactDetails.flatMap(_.addressLine1).getOrElse(fail)
            paragraphs.get(11).text() shouldBe consignee.contactDetails.flatMap(_.addressLine2).getOrElse(fail)
            paragraphs.get(12).text() shouldBe consignee.contactDetails.flatMap(_.addressLine3).getOrElse(fail)
            paragraphs.get(13).text() shouldBe consignee.contactDetails.flatMap(_.postalCode).getOrElse(fail)
            paragraphs.size()         shouldBe 15
          }
        )
      }

      "not all mandatory data from Acc14 is available, no contact address is shown" in forAll(journeys) { journey =>
        val acc14 = generateAcc14WithAddresses()

        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey)),
          None,
          None
        )
        val fillingOutClaim = removeContactDetails(foc)

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
            paragraphs.get(1).text() shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail.value
            paragraphs.get(2).text() shouldBe consignee.establishmentAddress.addressLine1
            paragraphs.get(3).text() shouldBe consignee.establishmentAddress.addressLine2.getOrElse(fail)
            paragraphs.get(4).text() shouldBe consignee.establishmentAddress.addressLine3.getOrElse(fail)
            paragraphs.get(5).text() shouldBe consignee.establishmentAddress.postalCode.getOrElse(fail)
            paragraphs.size()        shouldBe 6
          }
        )
      }
    }

    "handle add submit requests" when {

      def submitAdd(data: Seq[(String, String)], journeyBindable: JourneyBindable): Future[Result] =
        controller.add(journeyBindable)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "user chooses the Yes option" in forAll(journeys) { journey =>
        val acc14           = generateAcc14WithAddresses()
        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey))
        )
        val fillingOutClaim = removeContactDetails(foc)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkIsRedirect(
          submitAdd(Seq(languageKey -> "0"), journey),
          routes.EnterOrChangeContactDetailsController.enterMrnContactDetails(journey)
        )
      }

      "user chooses the No option" in forAll(journeys) { journey =>
        val acc14           = generateAcc14WithAddresses()
        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey))
        )
        val fillingOutClaim = removeContactDetails(foc)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkIsRedirect(
          submitAdd(Seq(languageKey -> "1"), journey),
          routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds()
        )
      }
      "the user does not select an option" in forAll(journeys) { journey =>
        val acc14           = generateAcc14WithAddresses()
        val (session, foc)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey))
        )
        val fillingOutClaim = removeContactDetails(foc)

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
          val fillingOutClaim = removeContactDetails(foc)

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
        controller.change(journeyBindable)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "user chooses the Yes option" in forAll(journeys) { journey =>
        val acc14   = generateAcc14WithAddresses()
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey))
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          submitChange(Seq(languageKey -> "0"), journey),
          routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds()
        )
      }

      "user chooses the No option" in forAll(journeys) { journey =>
        val acc14   = generateAcc14WithAddresses()
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey))
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          submitChange(Seq(languageKey -> "1"), journey),
          routes.CheckClaimantDetailsController.show(journey)
        )
      }
      "the user does not select an option" in forAll(journeys) { journey =>
        val acc14   = generateAcc14WithAddresses()
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.Importer),
          Some(toSelectNumberOfClaims(journey))
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
            Some(toSelectNumberOfClaims(journey))
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

  "CheckClaimantDetailsController Companion object" should {

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

      val contactDetails = extractContactDetails(fillingOutClaim)
      contactDetails.name                     shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.contactName)
      contactDetails.phoneNumber.map(_.value) shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail

      val contactAddress = extractContactAddress(fillingOutClaim)
      contactAddress.map(_.line1)        shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.addressLine1)
      contactAddress.flatMap(_.line2)    shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.addressLine2)
      contactAddress.map(_.postcode)     shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.postalCode)
      contactAddress.map(_.country.code) shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.countryCode)

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

      val contactDetails = extractContactDetails(fillingOutClaim)
      contactDetails.name                     shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.contactName)
      contactDetails.phoneNumber.map(_.value) shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail

      val contactAddress = extractContactAddress(fillingOutClaim)
      contactAddress.map(_.line1)        shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.addressLine1)
      contactAddress.flatMap(_.line2)    shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.addressLine2)
      contactAddress.map(_.postcode)     shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.postalCode)
      contactAddress.map(_.country.code) shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.countryCode)

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

      val contactDetails = extractContactDetails(fillingOutClaim)
      contactDetails.name                     shouldBe acc14Declarant.contactDetails.flatMap(_.contactName)
      contactDetails.phoneNumber.map(_.value) shouldBe acc14Declarant.contactDetails.flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail

      val contactAddress = extractContactAddress(fillingOutClaim)
      contactAddress.map(_.line1)        shouldBe acc14Declarant.contactDetails.flatMap(_.addressLine1)
      contactAddress.flatMap(_.line2)    shouldBe acc14Declarant.contactDetails.flatMap(_.addressLine2)
      contactAddress.map(_.postcode)     shouldBe acc14Declarant.contactDetails.flatMap(_.postalCode)
      contactAddress.map(_.country.code) shouldBe acc14Declarant.contactDetails.flatMap(_.countryCode)
    }

    "Extract ContactDetails and ContactAddress from session store instead of Acc14" in {
      val acc14           = generateAcc14WithAddresses()
      val contactDetails  = sample[MrnContactDetails]
      val contactAddress  = sample[NonUkAddress]
      val fillingOutClaim =
        getSessionWithPreviousAnswer(
          Some(acc14),
          Some(DeclarantTypeAnswer.AssociatedWithRepresentativeCompany),
          Some(toSelectNumberOfClaims(JourneyBindable.Single)),
          Some(contactDetails),
          Some(contactAddress)
        )._2

      val extractedContactDetails = extractContactDetails(fillingOutClaim)
      extractedContactDetails.name.getOrElse(fail)    shouldBe contactDetails.fullName
      extractedContactDetails.phoneNumber             shouldBe contactDetails.phoneNumber
      extractedContactDetails.email.getOrElse(fail()) shouldBe contactDetails.emailAddress

      val extractedContactAddress = extractContactAddress(fillingOutClaim)
      extractedContactAddress.map(_.line1).getOrElse(fail)    shouldBe contactAddress.line1
      extractedContactAddress.flatMap(_.line2)                shouldBe contactAddress.line2
      extractedContactAddress.map(_.postcode).getOrElse(fail) shouldBe contactAddress.postcode
      extractedContactAddress.map(_.country).getOrElse(fail)  shouldBe contactAddress.country

    }

  }

  "Validating session and acc14 data" should {

    "return true if we have valid session data" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single)),
        Some(sample[MrnContactDetails]),
        Some(sample[NonUkAddress])
      )._2

      isMandatoryDataAvailable(fillingOutClaim) shouldBe true
    }

    "return false if we have valid contact details but missing contact address" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single)),
        Some(sample[MrnContactDetails]),
        None
      )._2

      isMandatoryDataAvailable(fillingOutClaim) shouldBe false

    }
    "return false if we have valid contact address but missing contact details" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(DeclarantTypeAnswer.Importer),
        Some(toSelectNumberOfClaims(JourneyBindable.Single)),
        None,
        Some(sample[NonUkAddress])
      )._2

      isMandatoryDataAvailable(fillingOutClaim) shouldBe false

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

      isMandatoryDataAvailable(fillingOutClaim) shouldBe true
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

      isMandatoryDataAvailable(fillingOutClaim) shouldBe false
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

      isMandatoryDataAvailable(fillingOutClaim) shouldBe false

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

      isMandatoryDataAvailable(fillingOutClaim) shouldBe false

    }

  }
}
