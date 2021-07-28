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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarantTypeAnswer, SessionData, SignedInUserDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PhoneNumberGen._
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
    displayDeclaration: Option[DisplayDeclaration]
  ): (SessionData, FillingOutClaim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(
      displayDeclaration = displayDeclaration
    )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey
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

  implicit class ExtractFillingOutClaimClass(sessionData: SessionData) {
    def extractFillingOutClaim(): FillingOutClaim =
      sessionData.journeyStatus match {
        case Some(f @ FillingOutClaim(_, _, _)) => f
        case _                                  => fail("Failed to update DisplayResponseDetail")
      }
  }

  "CheckClaimantDetailsController" must {

    def performAction(journey: JourneyBindable): Future[Result] = controller.show(journey)(FakeRequest())

    "redirect to the start of the journey" when {
      "there is no journey status in the session" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(None)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(journey),
          baseRoutes.StartController.start()
        )
      }
    }

    "display the page" in forAll(journeys) { journey =>
      val contactDetails       = sample[ContactDetails].copy(telephone = Some(sample[PhoneNumber].value))
      val establishmentAddress = sample[EstablishmentAddress]
      val consignee            = sample[ConsigneeDetails]
        .copy(establishmentAddress = establishmentAddress, contactDetails = Some(contactDetails))
      val displayDeclaration   = Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consignee)))
      )
      val session              =
        getSessionWithPreviousAnswer(Some(displayDeclaration))._1
          .withDeclarantType(DeclarantTypeAnswer.Importer)
          .withAcc14Data(displayDeclaration.displayResponseDetail)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(journey),
        messageFromMessageKey("claimant-details.title"),
        doc => {
          val paragraphs = doc.select("dd > p")
          paragraphs.get(0).text() shouldBe consignee.legalName
          paragraphs.get(1).text() shouldBe consignee.contactDetails.flatMap(_.telephone).getOrElse(fail())
        }
      )
    }
  }

  "CheckClaimantDetailsController Companion object" should {

    "extract contact details for Importer" in {
      val contactDetails             = sample[ContactDetails]
      val establishmentAddress       = sample[EstablishmentAddress]
      val consignee                  = sample[ConsigneeDetails]
        .copy(establishmentAddress = establishmentAddress, contactDetails = Some(contactDetails))
      val displayDeclaration         = Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consignee)))
      )
      val (session, fillingOutClaim) = getSessionWithPreviousAnswer(Some(displayDeclaration))

      val foc = session
        .withDeclarantType(DeclarantTypeAnswer.Importer)
        .withAcc14Data(displayDeclaration.displayResponseDetail)
        .extractFillingOutClaim

      val namePhoneEmail = extractContactsRegisteredWithCDSA(foc)
      namePhoneEmail.name.getOrElse(fail())   shouldBe consignee.legalName
      namePhoneEmail.phoneNumber.map(_.value) shouldBe consignee.contactDetails.flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail

      val address = extractEstablishmentAddress(foc)
      address.getOrElse(fail) shouldBe establishmentAddress
    }

    "extract contact details and address for Representative Company" in {
      val contactDetails             = sample[ContactDetails]
      val establishmentAddress       = sample[EstablishmentAddress]
      val declarant                  = sample[DeclarantDetails]
        .copy(establishmentAddress = establishmentAddress, contactDetails = Some(contactDetails))
      val displayDeclaration         = Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(declarantDetails = declarant))
      )
      val (session, fillingOutClaim) = getSessionWithPreviousAnswer(Some(displayDeclaration))

      val foc = session
        .withDeclarantType(DeclarantTypeAnswer.AssociatedWithRepresentativeCompany)
        .withAcc14Data(displayDeclaration.displayResponseDetail)
        .extractFillingOutClaim

      val namePhoneEmail = extractContactsRegisteredWithCDSA(foc)
      namePhoneEmail.name.getOrElse(fail())   shouldBe declarant.legalName
      namePhoneEmail.phoneNumber.map(_.value) shouldBe declarant.contactDetails.flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe fillingOutClaim.signedInUserDetails.verifiedEmail

      val address = extractEstablishmentAddress(foc)
      address.getOrElse(fail) shouldBe establishmentAddress
    }

  }

}
