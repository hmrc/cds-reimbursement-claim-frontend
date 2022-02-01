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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.Name
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnrolmentConfig.EoriEnrolment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AddressLookupSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.buildCompleteJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.displayDeclarationGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen.genEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.genUrl
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import java.util.UUID
import cats.implicits.catsSyntaxEq
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen.genMrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.RetrievedUserTypeGen.individualGen
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future

class CheckClaimantDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with AddressLookupSupport
    with ScalaCheckPropertyChecks
    with RejectedGoodsSingleJourneyTestData {

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
    featureSwitch.enable(Feature.RejectedGoods)

  private val session = SessionData.empty.copy(
    rejectedGoodsSingleJourney = Some(RejectedGoodsSingleJourney.empty(exampleEori))
  )

  "Check Claimant Details Controller" when {
    "Show Check Claimant Details page" must {

      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        forAll(buildCompleteJourneyGen(), genEmail, genName) { (journey, email, name) =>
          val sessionToAmend = session.copy(rejectedGoodsSingleJourney = Some(journey))

          inSequence {
            mockAuthWithAllRetrievals(
              Some(AffinityGroup.Individual),
              Some(email.value),
              Set(
                Enrolment(EoriEnrolment.key)
                  .withIdentifier(EoriEnrolment.eoriEnrolmentIdentifier, journey.getClaimantEori.value)
              ),
              Some(Credentials("id", "GovernmentGateway")),
              Some(Name(name.name, name.lastName))
            )
            mockGetSession(sessionToAmend)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("check-claimant-details.title"),
            doc => doc.select("form").attr("action") shouldBe routes.CheckClaimantDetailsController.submit().url
          )
        }
      }

      "redirect to the Mrn Entry page if no Acc14 response obtained yet" in {
        forAll(genEmail, genName, genEori) { (email, name, eori) =>
          inSequence {
            mockAuthWithAllRetrievals(
              Some(AffinityGroup.Individual),
              Some(email.value),
              Set(Enrolment(EoriEnrolment.key).withIdentifier(EoriEnrolment.eoriEnrolmentIdentifier, eori.value)),
              Some(Credentials("id", "GovernmentGateway")),
              Some(Name(name.name, name.lastName))
            )
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.EnterMovementReferenceNumberController.show()
          )
        }
      }
    }

    "Submit Check Claimant Detials page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "redirect to the basis for claims page and do not update the contact/address details if they are already present" in {
        forAll(displayDeclarationGen, genEmail, genName, genMrnContactDetails, genContactAddress) {
          (displayDeclaration, email, name, contactDeatils, address) =>
            val journey = RejectedGoodsSingleJourney
              .empty(exampleEori)
              .submitDisplayDeclaration(displayDeclaration)
              .submitContactDetails(Some(contactDeatils))
              .submitContactAddress(address)
            val session = SessionData.empty.copy(
              rejectedGoodsSingleJourney = Some(journey)
            )

            inSequence {
              mockAuthWithAllRetrievals(
                Some(AffinityGroup.Individual),
                Some(email.value),
                Set(
                  Enrolment(EoriEnrolment.key)
                    .withIdentifier(EoriEnrolment.eoriEnrolmentIdentifier, journey.getClaimantEori.value)
                ),
                Some(Credentials("id", "GovernmentGateway")),
                Some(Name(name.name, name.lastName))
              )
              mockGetSession(session)
            }

            checkIsRedirect(
              performAction(),
              routes.BasisForClaimController.show()
            )
        }
      }

      "redirect to the basis for claims page and update the contact/address details if the journey does not already contain them." in {
        forAll(displayDeclarationGen, genEmail, genName, individualGen, genConsigneeDetails, genDeclarantDetails) {
          (initialDisplayDeclaration, email, name, individual, consignee, declarant) =>
            val eori               = exampleEori
            val drd                = initialDisplayDeclaration.displayResponseDetail.copy(
              declarantDetails = declarant.copy(declarantEORI = eori.value),
              consigneeDetails = Some(consignee.copy(consigneeEORI = eori.value))
            )
            val displayDeclaration = initialDisplayDeclaration.copy(displayResponseDetail = drd)
            val journey            = RejectedGoodsSingleJourney
              .empty(exampleEori)
              .submitDisplayDeclaration(displayDeclaration)
            val session            = SessionData.empty.copy(
              rejectedGoodsSingleJourney = Some(journey)
            )

            val expectedContactDetails = journey.computeContactDetails(individual)
            val expectedAddress        = journey.computeAddressDetails.get
            val expectedJourney        =
              journey.submitContactDetails(expectedContactDetails).submitContactAddress(expectedAddress)
            val updatedSession         = session.copy(rejectedGoodsSingleJourney = Some(expectedJourney))

            inSequence {
              mockAuthWithAllRetrievals(
                Some(AffinityGroup.Individual),
                Some(email.value),
                Set(Enrolment(EoriEnrolment.key).withIdentifier(EoriEnrolment.eoriEnrolmentIdentifier, eori.value)),
                Some(Credentials("id", "GovernmentGateway")),
                Some(Name(name.name, name.lastName))
              )
              mockGetSession(session)
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.BasisForClaimController.show()
            )
        }
      }

      "redirect to the enter MRN page if no contact details present" in {
        forAll(displayDeclarationGen, genEmail, genName) { (displayDeclaration, email, name) =>
          whenever(
            displayDeclaration.getConsigneeDetails.get.consigneeEORI =!= exampleEori.value &&
              displayDeclaration.getDeclarantDetails.declarantEORI =!= exampleEori.value
          ) {
            val journey = RejectedGoodsSingleJourney
              .empty(exampleEori)
              .submitDisplayDeclaration(displayDeclaration)
            val session = SessionData.empty.copy(
              rejectedGoodsSingleJourney = Some(journey)
            )

            inSequence {
              mockAuthWithAllRetrievals(
                Some(AffinityGroup.Individual),
                Some(email.value),
                Set(
                  Enrolment(EoriEnrolment.key)
                    .withIdentifier(EoriEnrolment.eoriEnrolmentIdentifier, journey.getClaimantEori.value)
                ),
                Some(Credentials("id", "GovernmentGateway")),
                Some(Name(name.name, name.lastName))
              )
              mockGetSession(session)
            }

            checkIsRedirect(
              performAction(),
              routes.EnterMovementReferenceNumberController.show()
            )
          }
        }
      }
    }
  }

  "The address lookup" should {

    "start successfully" in forAll(genUrl) { lookupUrl =>
      inSequence {
        mockAuthWithNoRetrievals()
        mockAddressLookup(Right(lookupUrl))
      }

      checkIsRedirect(startAddressLookup(), lookupUrl.toString)
    }

    "fail to start if error response received from downstream ALF service" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockAddressLookup(Left(Error("Request was not accepted")))
      }

      checkIsTechnicalErrorPage(startAddressLookup())
    }

    "update an address once complete" in forAll(genContactAddress) { address =>
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Right(address))
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        retrieveAddress(Some(UUID.randomUUID())),
        routes.CheckClaimantDetailsController.show()
      )
    }

    "fail to update address once bad address lookup ID provided" in {
      val addressId = UUID.randomUUID()

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Left(Error(s"No address found for $addressId")))
      }

      checkIsRedirect(
        retrieveAddress(Some(addressId)),
        baseRoutes.IneligibleController.ineligible()
      )
    }

    def startAddressLookup(): Future[Result] =
      controller.redirectToALF(FakeRequest())

    def retrieveAddress(maybeAddressId: Option[UUID]): Future[Result] =
      controller.retrieveAddressFromALF(maybeAddressId)(FakeRequest())
  }
}
