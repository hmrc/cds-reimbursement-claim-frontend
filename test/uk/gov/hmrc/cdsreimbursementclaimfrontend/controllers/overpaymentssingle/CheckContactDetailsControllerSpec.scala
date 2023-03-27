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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import cats.Functor
import cats.Id
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.{routes => overpaymentsSingleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AddressLookupSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimantInformationSummary

import java.net.URL
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

class CheckContactDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AddressLookupSupport
    with BeforeAndAfterEach
    with SummaryMatchers
    with ScalaCheckPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[AddressLookupService].toInstance(addressLookupServiceMock)
    )

  val controller: CheckContactDetailsController = instanceOf[CheckContactDetailsController]
  val journey: JourneyBindable                  = JourneyBindable.Single

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private def getSessionWithPreviousAnswer(
    displayDeclaration: Option[DisplayDeclaration],
    maybeTypeOfClaim: Option[TypeOfClaimAnswer],
    mrnContactDetailsAnswer: Option[MrnContactDetails] = None,
    mrnContactAddressAnswer: Option[ContactAddress] = None
  ): (SessionData, FillingOutClaim) = {
    val signedInUserDetails = sample[SignedInUserDetails]
    val draftC285Claim      = DraftClaim.blank.copy(
      displayDeclaration = displayDeclaration.map(_.withConsigneeEori(signedInUserDetails.eori)),
      typeOfClaim = maybeTypeOfClaim,
      movementReferenceNumber = Some(sample[MRN]),
      mrnContactDetailsAnswer = mrnContactDetailsAnswer,
      mrnContactAddressAnswer = mrnContactAddressAnswer
    )
    val ggCredId            = sample[GGCredId]
    val fillingOutClaim     = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)

    (SessionData(fillingOutClaim), fillingOutClaim)
  }

  "Check Contact Details Controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {
        val session = getSessionWithPreviousAnswer(None, Some(toTypeOfClaim(journey)))._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          controller.show()(FakeRequest()),
          baseRoutes.StartController.start()
        )
      }

    }

    "display the page" when {

      "contact and address data answers are available" in {

        val acc14             = genAcc14WithAddresses
        val mrnContactDetails = sample[MrnContactDetails].copy(phoneNumber = Some(sample[PhoneNumber]))
        val mrnContactAddress = sample[ContactAddress]
          .copy(line2 = Some(alphaCharGen(10)), line3 = Some(alphaCharGen(10)), country = Country.uk)

        val (session, fillingOutClaim) = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(toTypeOfClaim(journey)),
          Some(mrnContactDetails),
          Some(mrnContactAddress)
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("claimant-details.title"),
          doc => {
            val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
            val summaryValues = doc.select(".govuk-summary-list__value").eachText()
            val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

            summaries.toSeq should containOnlyDefinedPairsOf(
              Seq(
                "Contact details" -> fillingOutClaim.draftClaim
                  .getClaimantInformation(fillingOutClaim.signedInUserDetails.eori)
                  .map(ClaimantInformationSummary.getContactDataString),
                "Contact address" -> fillingOutClaim.draftClaim
                  .getClaimantInformation(fillingOutClaim.signedInUserDetails.eori)
                  .map(ClaimantInformationSummary.getAddressDataString)
              )
            )
          }
        )
      }

      "consignee contact and address data from ACC14 is available" in {

        val acc14 = genAcc14WithAddresses

        val (session, fillingOutClaim) = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(toTypeOfClaim(journey))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("claimant-details.title"),
          doc => {
            val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
            val summaryValues = doc.select(".govuk-summary-list__value").eachText()
            val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

            summaries.toSeq should containOnlyDefinedPairsOf(
              Seq(
                "Contact details" -> fillingOutClaim.draftClaim
                  .computeClaimantInformation(fillingOutClaim.signedInUserDetails)
                  .map(ClaimantInformationSummary.getContactDataString),
                "Contact address" -> fillingOutClaim.draftClaim
                  .computeClaimantInformation(fillingOutClaim.signedInUserDetails)
                  .map(ClaimantInformationSummary.getAddressDataString)
              )
            )
          }
        )
      }

      "declarant contact and address data from ACC14 is available" in {
        val acc14 = genAcc14WithoutContactDetails

        val (session, fillingOutClaim) = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(toTypeOfClaim(journey)),
          None,
          None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("claimant-details.title"),
          doc => {
            val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
            val summaryValues = doc.select(".govuk-summary-list__value").eachText()
            val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

            summaries.toSeq should containOnlyDefinedPairsOf(
              Seq(
                "Contact details" -> fillingOutClaim.draftClaim
                  .computeClaimantInformation(fillingOutClaim.signedInUserDetails)
                  .map(ClaimantInformationSummary.getContactDataString),
                "Contact address" -> fillingOutClaim.draftClaim
                  .computeClaimantInformation(fillingOutClaim.signedInUserDetails)
                  .map(ClaimantInformationSummary.getAddressDataString)
              )
            )
          }
        )
      }

      "not consignee nor declarant contact and address data from ACC14 is available" in {
        val acc14 = genAcc14WithoutConsigneeAndDeclarantDetails

        val (session, fillingOutClaim) = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(toTypeOfClaim(journey)),
          None,
          None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("claimant-details.title"),
          doc => {
            val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
            val summaryValues = doc.select(".govuk-summary-list__value").eachText()
            val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

            summaries.toSeq should containOnlyDefinedPairsOf(
              Seq(
                "Contact details" -> fillingOutClaim.draftClaim
                  .computeClaimantInformation(fillingOutClaim.signedInUserDetails)
                  .map(ClaimantInformationSummary.getContactDataString),
                "Contact address" -> fillingOutClaim.draftClaim
                  .computeClaimantInformation(fillingOutClaim.signedInUserDetails)
                  .map(ClaimantInformationSummary.getAddressDataString)
              )
            )
          }
        )
      }
    }

    "handle submit requests" when {

      "when contact and address answers provided" in {
        val acc14   = genAcc14WithAddresses
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(toTypeOfClaim(journey)),
          Some(sample[MrnContactDetails]),
          Some(sample[ContactAddress].copy(country = Country.uk))
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.submit()(FakeRequest()),
          OverpaymentsRoutes.NorthernIrelandController.show(journey)
        )
      }

      "when contact and address answers not yet provided" in {
        val acc14   = genAcc14WithAddresses
        val session = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(toTypeOfClaim(journey))
        )._1

        val updatedSession = session
          .copy(journeyStatus =
            session.journeyStatus
              .map {
                case claim: FillingOutClaim =>
                  claim.copy(draftClaim =
                    claim.draftClaim.copy(
                      mrnContactDetailsAnswer = claim.draftClaim.computeContactDetails(claim.signedInUserDetails),
                      mrnContactAddressAnswer = claim.draftClaim.computeAddressDetails(claim.signedInUserDetails)
                    )
                  )
                case _                      =>
                  fail("wrong C285 claim status")
              }
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          controller.submit(FakeRequest()),
          OverpaymentsRoutes.NorthernIrelandController.show(journey)
        )
      }

    }

    "redirect to the problem page" when {
      def updateAddress(maybeAddressId: Option[UUID]): Future[Result] =
        controller.retrieveAddressFromALF(maybeAddressId)(FakeRequest())

      "user chooses an address without a post code" in forAll { (id: UUID, journey: JourneyBindable) =>
        val acc14         = genAcc14WithAddresses
        val (session, _)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(toTypeOfClaim(journey))
        )
        val errorResponse = Error("parsing address lookup response:/address/postcode: error.path.missing")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockAddressRetrieve(Left(errorResponse))
        }

        checkIsRedirect(
          updateAddress(Some(id)),
          overpaymentsSingleRoutes.ProblemWithAddressController.show
        )
      }

      "user chooses an address without an address line 1" in forAll { (id: UUID, journey: JourneyBindable) =>
        val acc14         = genAcc14WithAddresses
        val (session, _)  = getSessionWithPreviousAnswer(
          Some(acc14),
          Some(toTypeOfClaim(journey))
        )
        val errorResponse = Error("parsing address lookup response:/address/lines: error.minLength")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockAddressRetrieve(Left(errorResponse))
        }

        checkIsRedirect(
          updateAddress(Some(id)),
          overpaymentsSingleRoutes.ProblemWithAddressController.show
        )
      }
    }

  }

  "Validating session and acc14 data" should {

    "return true if we have valid session data" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(toTypeOfClaim(JourneyBindable.Single)),
        Some(sample[MrnContactDetails]),
        Some(sample[ContactAddress])
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe true
    }

    "return false if we have valid contact details but missing contact address" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
        Some(toTypeOfClaim(JourneyBindable.Single)),
        Some(sample[MrnContactDetails]),
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe false

    }
    "return false if we have valid contact address but missing contact details" in {
      val fillingOutClaim = getSessionWithPreviousAnswer(
        None,
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
        Some(toTypeOfClaim(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe false
    }

    "return false if we have no session data, and no contact name in Acc14 data" in {
      val fullAcc14        = genAcc14WithAddresses
      val consigneeDetails = Functor[Id].map(fullAcc14.displayResponseDetail.consigneeDetails.getOrElse(fail()))(cd =>
        cd.copy(contactDetails = cd.contactDetails.map(contact => contact.copy(contactName = None)))
      )
      val acc14            = Functor[Id].map(fullAcc14)(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
      )
      val fillingOutClaim  = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(toTypeOfClaim(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe false
    }

    "return false if we have no session data, and no contact address line1 in Acc14 data" in {
      val fullAcc14        = genAcc14WithAddresses
      val consigneeDetails = Functor[Id].map(fullAcc14.displayResponseDetail.consigneeDetails.getOrElse(fail()))(cd =>
        cd.copy(contactDetails = cd.contactDetails.map(contact => contact.copy(addressLine1 = None)))
      )
      val acc14            = Functor[Id].map(fullAcc14)(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
      )
      val fillingOutClaim  = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(toTypeOfClaim(JourneyBindable.Single)),
        None,
        None
      )._2

      fillingOutClaim.draftClaim.isMandatoryContactDataAvailable shouldBe false

    }

    "return false if we have no session data, and no contact address postcode in Acc14 data" in {
      val fullAcc14        = genAcc14WithAddresses
      val consigneeDetails = Functor[Id].map(fullAcc14.displayResponseDetail.consigneeDetails.getOrElse(fail()))(cd =>
        cd.copy(contactDetails = cd.contactDetails.map(contact => contact.copy(postalCode = None)))
      )
      val acc14            = Functor[Id].map(fullAcc14)(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
      )
      val fillingOutClaim  = getSessionWithPreviousAnswer(
        Some(acc14),
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

      inSequence {
        mockAuthWithNoRetrievals()
        mockAddressLookup(Right(lookupUrl))
      }

      checkIsRedirect(
        controller.redirectToALF(FakeRequest()),
        lookupUrl.toString
      )
    }

    "fail to start if error response is received from downstream ALF service" in {

      inSequence {
        mockAuthWithNoRetrievals()
        mockAddressLookup(Left(Error("Request was not accepted")))
      }

      checkIsTechnicalErrorPage(controller.redirectToALF(FakeRequest()))
    }

    "update an address once complete" in {
      val id           = sample[UUID]
      val address      = sample[ContactAddress]
      val acc14        = genAcc14WithAddresses
      val (session, _) = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(toTypeOfClaim(journey))
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Right(address))
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        controller.retrieveAddressFromALF(Some(id))(FakeRequest()),
        routes.CheckContactDetailsController.show
      )
    }

    "fail to update address once bad address lookup ID provided" in {
      val id    = sample[UUID]
      val acc14 = genAcc14WithAddresses

      val (session, _) = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(toTypeOfClaim(journey))
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Left(Error(s"No address found for $id")))
      }

      checkIsTechnicalErrorPage(controller.retrieveAddressFromALF(Some(id))(FakeRequest()))
    }

    "redirect to show page once address lookup ID is not provided" in {
      val acc14        = genAcc14WithAddresses
      val (session, _) = getSessionWithPreviousAnswer(
        Some(acc14),
        Some(toTypeOfClaim(journey))
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkIsRedirect(
        controller.retrieveAddressFromALF(None)(FakeRequest()),
        routes.CheckContactDetailsController.show
      )
    }
  }
}
