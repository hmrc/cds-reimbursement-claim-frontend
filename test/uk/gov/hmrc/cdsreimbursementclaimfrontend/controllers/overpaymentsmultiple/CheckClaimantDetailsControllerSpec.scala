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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

import org.scalatest.BeforeAndAfterEach
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AddressLookupSupport, AuthSupport, PropertyBasedControllerSpec, SessionSupport, routes as baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen.genMrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.genUrl
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{AddressLookupService, FeatureSwitchService}

import java.util.UUID
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

class CheckClaimantDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with AddressLookupSupport {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[AddressLookupService].toInstance(addressLookupServiceMock)
    )

  val controller: CheckClaimantDetailsController = instanceOf[CheckClaimantDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  private val session = SessionData(journeyWithMrnAndDeclaration)

  "Check Claimant Details Controller" when {
    "Show Check Claimant Details page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        forAll(buildCompleteJourneyGen()) { journey =>
          val sessionToAmend = SessionData(journey)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(sessionToAmend)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("check-claimant-details.title"),
            doc => doc.select("form").attr("action") shouldBe routes.CheckClaimantDetailsController.submit.url
          )
        }
      }

      "redirect to the Mrn Entry page if no Acc14 response obtained yet" in {
        forAll(genEori) { eori =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(OverpaymentsMultipleJourney.empty(eori)))
          }

          checkIsRedirect(
            performAction(),
            routes.EnterMovementReferenceNumberController.showFirst
          )
        }
      }
    }

    "Submit Check Claimant Details page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "redirect to the select basis for claim page and do not update the contact/address details if they are already present" in {
        forAll(displayDeclarationGen, genMrnContactDetails, genContactAddress) {
          (displayDeclaration, contactDeatils, address) =>
            val journey = OverpaymentsMultipleJourney
              .empty(displayDeclaration.getDeclarantEori)
              .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
              .map(_.submitContactDetails(Some(contactDeatils)))
              .map(_.submitContactAddress(address))
              .getOrFail

            val session = SessionData(journey)

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(session)
            }

            checkIsRedirect(
              performAction(),
              routes.BasisForClaimController.show
            )
        }
      }

      "redirect to the select basis for claim page and update the contact/address details if the journey does not already contain them." in {
        forAll(displayDeclarationGen, genConsigneeDetails, genDeclarantDetails, genContactAddress) {
          (initialDisplayDeclaration, consignee, declarant, address) =>
            val eori               = exampleEori
            val drd                = initialDisplayDeclaration.displayResponseDetail.copy(
              declarantDetails = declarant.copy(declarantEORI = eori.value),
              consigneeDetails = Some(consignee.copy(consigneeEORI = eori.value))
            )
            val displayDeclaration = initialDisplayDeclaration.copy(displayResponseDetail = drd)
            val journey            = OverpaymentsMultipleJourney
              .empty(exampleEori)
              .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
              .getOrFail

            val session = SessionData(journey)

            val expectedContactDetails = journey.answers.contactDetails

            val expectedJourney =
              journey.submitContactDetails(expectedContactDetails).submitContactAddress(address)
            val updatedSession  = SessionData(expectedJourney)

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(session)
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.BasisForClaimController.show
            )
        }
      }

      "redirect to the basis for claims page and update the contact/address details if third party user" in {
        forAll(displayDeclarationGen, genEori) { (displayDeclaration, userEori) =>
          val journey = OverpaymentsMultipleJourney
            .empty(userEori)
            .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
            .flatMap(_.submitConsigneeEoriNumber(displayDeclaration.getConsigneeEori.get))
            .flatMap(_.submitDeclarantEoriNumber(displayDeclaration.getDeclarantEori))
            .getOrFail

          val session = SessionData(journey)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(session)
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performAction(),
            routes.BasisForClaimController.show
          )
        }
      }
    }
  }

  "The address lookup" should {

    "start successfully" in forAll(genUrl) { lookupUrl =>
      inSequence {
        mockAuthWithDefaultRetrievals()
        mockAddressLookup(Right(lookupUrl))
      }

      checkIsRedirect(startAddressLookup(), lookupUrl.toString)
    }

    "fail to start if error response received from downstream ALF service" in {
      inSequence {
        mockAuthWithDefaultRetrievals()
        mockAddressLookup(Left(Error("Request was not accepted")))
      }

      checkIsTechnicalErrorPage(startAddressLookup())
    }

    "update an address once complete" in forAll(genContactAddress) { address =>
      inSequence {
        mockAuthWithDefaultRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Right(address))
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        retrieveAddress(Some(UUID.randomUUID())),
        routes.CheckClaimantDetailsController.show
      )
    }

    "fail to update address once bad address lookup ID provided" in {
      val addressId = UUID.randomUUID()

      inSequence {
        mockAuthWithDefaultRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Left(Error(s"No address found for $addressId")))
      }

      checkIsRedirect(
        retrieveAddress(Some(addressId)),
        baseRoutes.IneligibleController.ineligible
      )
    }

    def startAddressLookup(): Future[Result] =
      controller.redirectToALF(FakeRequest())

    def retrieveAddress(maybeAddressId: Option[UUID]): Future[Result] =
      controller.retrieveAddressFromALF(maybeAddressId)(FakeRequest())
  }
}
