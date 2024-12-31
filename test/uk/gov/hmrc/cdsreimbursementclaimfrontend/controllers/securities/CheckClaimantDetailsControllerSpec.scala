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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import org.scalatest.BeforeAndAfterEach
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AddressLookupSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen.genMrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.genUrl
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future

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

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.Securities)
    featureSwitch.disable(Feature.LimitedAccessSecurities)
  }

  "Check Claimant Details Controller" when {
    "Show Check Claimant Details page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        forAll(mrnWithtRfsWithDisplayDeclarationGen, genMrnContactDetails, genContactAddress) {
          case ((mrn, rfs, decl), contactDeatils, address) =>
            val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
            whenever(depositIds.nonEmpty) {
              val journey = SecuritiesJourney
                .empty(exampleEori)
                .submitMovementReferenceNumber(mrn)
                .submitReasonForSecurityAndDeclaration(rfs, decl)
                .flatMap(_.submitClaimDuplicateCheckStatus(false))
                .flatMap(_.selectSecurityDepositId(depositIds.head))
                .map(_.submitContactDetails(Some(contactDeatils)))
                .map(_.submitContactAddress(address))
                .getOrFail

              val session = SessionData(journey)

              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(session)
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("check-claimant-details.title"),
                doc => doc.select("form").attr("action") shouldBe routes.CheckClaimantDetailsController.submit.url
              )
            }
        }
      }

      "redirect to the Mrn Entry page if no Acc14 response obtained yet" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(SecuritiesJourney.empty(exampleEori)))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterMovementReferenceNumberController.show
        )
      }
    }

    "Submit Check Claimant Details page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "redirect to the first confirm full repayment page and do not update the contact/address details if they are already present" in {
        forAll(
          mrnWithtRfsWithDisplayDeclarationGen,
          genMrnContactDetails,
          genContactAddress
        ) { case ((mrn, rfs, decl), contactDeatils, address) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty) {
            val journey = SecuritiesJourney
              .empty(exampleEori)
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositId(depositIds.head))
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
              routes.ConfirmFullRepaymentController.showFirst
            )
          }
        }
      }

      "redirect to the first confirm full repayment page and update the contact/address details if the journey does not already contain them." in {
        forAll(
          mrnWithtRfsWithDisplayDeclarationGen,
          genConsigneeDetails,
          genDeclarantDetails,
          genContactAddress
        ) { case ((mrn, rfs, decl), consignee, declarant, address) =>
          val eori               = exampleEori
          val drd                = decl.displayResponseDetail.copy(
            declarantDetails = declarant.copy(declarantEORI = eori.value),
            consigneeDetails = Some(consignee.copy(consigneeEORI = eori.value))
          )
          val displayDeclaration = decl.copy(displayResponseDetail = drd)
          val depositIds         = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          val journey            = SecuritiesJourney
            .empty(exampleEori)
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, displayDeclaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositId(depositIds.head))
            .getOrFail

          val session = SessionData(journey)

          val expectedContactDetails = journey.answers.contactDetails
          val expectedJourney        =
            journey.submitContactDetails(expectedContactDetails).submitContactAddress(address)
          val updatedSession         = SessionData(expectedJourney)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(),
            routes.ConfirmFullRepaymentController.showFirst
          )
        }
      }

      "redirect to the basis for claims page and update the contact/address details if third party user" in {
        forAll(mrnWithtRfsWithDisplayDeclarationGen, genEori) { case ((mrn, rfs, decl), userEori) =>
          val declarationWithoutContactDetails =
            decl.copy(displayResponseDetail =
              decl.displayResponseDetail.copy(
                declarantDetails = decl.displayResponseDetail.declarantDetails.copy(contactDetails = None),
                consigneeDetails = None
              )
            )

          val journey = SecuritiesJourney
            .empty(userEori)
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, declarationWithoutContactDetails)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.submitDeclarantEoriNumber(declarationWithoutContactDetails.getDeclarantEori))
            .getOrFail

          val session = SessionData(journey)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(session)
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performAction(),
            routes.ConfirmFullRepaymentController.showFirst
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

    "update an address once complete" in forAll(genContactAddress, genReasonForSecurity) { (address, rfs) =>
      val initialJourney =
        securitiesJourneyWithMrnAndRfsAndDeclaration(rfs)

      inSequence {
        mockAuthWithDefaultRetrievals()
        mockGetSession(SessionData(initialJourney))
        mockAddressRetrieve(Right(address))
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        retrieveAddress(Some(UUID.randomUUID())),
        routes.CheckClaimantDetailsController.show
      )
    }

    "fail to update address once bad address lookup ID provided" in {
      val initialJourney =
        securitiesJourneyWithMrnAndRfsAndDeclaration(ReasonForSecurity.ManualOverrideDeposit)

      val addressId = UUID.randomUUID()

      inSequence {
        mockAuthWithDefaultRetrievals()
        mockGetSession(SessionData(initialJourney))
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
