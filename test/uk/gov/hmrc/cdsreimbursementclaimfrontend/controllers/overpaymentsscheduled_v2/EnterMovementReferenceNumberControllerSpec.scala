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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2

import cats.data.EitherT
import cats.implicits._
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status.BAD_REQUEST
import play.api.http.Status.NOT_FOUND
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarantDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterMovementReferenceNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with DeclarationSupport
    with BeforeAndAfterEach {

  val mockClaimService: ClaimService       = mock[ClaimService]
  val mockXiEoriConnector: XiEoriConnector = mock[XiEoriConnector]

  val enterMovementReferenceNumberKey: String = "enter-movement-reference-number"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService),
      bind[XiEoriConnector].toInstance(mockXiEoriConnector)
    )

  val controller: EnterMovementReferenceNumberController = instanceOf[EnterMovementReferenceNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  val session = SessionData.empty.copy(
    overpaymentsScheduledJourney = Some(OverpaymentsScheduledJourney.empty(exampleEori))
  )

  private def mockGetDisplayDeclaration(expectedMrn: MRN, response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(expectedMrn, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockGetXiEori(response: Future[UserXiEori]) =
    (mockXiEoriConnector
      .getXiEori(_: HeaderCarrier))
      .expects(*)
      .returning(response)

  "Movement Reference Number Controller" when {
    "Enter MRN page" must {

      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-movement-reference-number.scheduled.title"),
          doc => {
            doc.select("#enter-movement-reference-number").`val`() shouldBe ""
            doc.select("form").attr("action")                      shouldBe routes.EnterMovementReferenceNumberController.submit.url
          }
        )
      }

      "display the page on a pre-existing journey" in {
        val journey        = completeJourneyWithMatchingUserEoriGen.sample.getOrElse(
          fail("Unable to generate complete journey")
        )
        val mrn            = journey.answers.movementReferenceNumber.getOrElse(fail("No mrn found in journey"))
        val sessionToAmend = session.copy(overpaymentsScheduledJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-movement-reference-number.scheduled.title"),
          doc => doc.select("#enter-movement-reference-number").`val`() shouldBe mrn.value
        )
      }
    }

    "Submit MRN page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an invalid MRN" in {
        val invalidMRN = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(enterMovementReferenceNumberKey -> invalidMRN.value),
          messageFromMessageKey("enter-movement-reference-number.scheduled.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-movement-reference-number.invalid.number"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an empty MRN" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(enterMovementReferenceNumberKey -> ""),
          messageFromMessageKey("enter-movement-reference-number.scheduled.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-movement-reference-number.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "submit an unknown MRN" in forAll { (mrn: MRN) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(mrn, Right(None))
        }

        checkIsRedirect(
          performAction(enterMovementReferenceNumberKey -> mrn.value),
          baseRoutes.IneligibleController.ineligible()
        )
      }

      "submit a valid MRN and user is declarant" in forAll { (mrn: MRN) =>
        val journey                       = session.overpaymentsScheduledJourney.getOrElse(fail("No overpayments journey"))
        val displayDeclaration            = buildDisplayDeclaration().withDeclarationId(mrn.value)
        val updatedDeclarantDetails       = displayDeclaration.displayResponseDetail.declarantDetails.copy(
          declarantEORI = journey.answers.userEoriNumber.value
        )
        val updatedDisplayResponseDetails =
          displayDeclaration.displayResponseDetail.copy(declarantDetails = updatedDeclarantDetails)
        val updatedDisplayDeclaration     = displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
        val updatedJourney                =
          journey
            .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
            .getOrFail
        val updatedSession                = session.copy(overpaymentsScheduledJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(mrn, Right(Some(updatedDisplayDeclaration)))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(enterMovementReferenceNumberKey -> mrn.value),
          routes.CheckDeclarationDetailsController.show
        )
      }

      "submit a valid MRN and user is not the declarant nor consignee" in forAll {
        (mrn: MRN, declarant: Eori, consignee: Eori) =>
          whenever(declarant =!= exampleEori && consignee =!= exampleEori) {
            val journey            = session.overpaymentsScheduledJourney.getOrElse(fail("No overpayments journey"))
            val displayDeclaration = buildDisplayDeclaration().withDeclarationId(mrn.value)
            val declarantDetails   = sample[DeclarantDetails].copy(declarantEORI = declarant.value)
            val consigneeDetails   = sample[ConsigneeDetails].copy(consigneeEORI = consignee.value)

            val updatedDisplayResponseDetails = displayDeclaration.displayResponseDetail.copy(
              declarantDetails = declarantDetails,
              consigneeDetails = Some(consigneeDetails)
            )
            val updatedDisplayDeclaration     =
              displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
            val updatedJourney                =
              journey
                .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
                .getOrFail
            val updatedSession                = session.copy(overpaymentsScheduledJourney = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockGetDisplayDeclaration(mrn, Right(Some(updatedDisplayDeclaration)))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(enterMovementReferenceNumberKey -> mrn.value),
              routes.EnterImporterEoriNumberController.show
            )
          }
      }

      "reject an MRN with subsidies payment method" in forAll { (mrn: MRN, declarant: Eori, consignee: Eori) =>
        featureSwitch.enable(Feature.BlockSubsidies)

        val displayDeclaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false), (TaxCode.A70, 100, false)))
            .withDeclarationId(mrn.value)
            .withDeclarantEori(declarant)
            .withConsigneeEori(consignee)
            .withAllSubsidiesPaymentMethod()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(mrn, Right(Some(displayDeclaration)))
        }

        checkPageIsDisplayed(
          performAction(enterMovementReferenceNumberKey -> mrn.value),
          messageFromMessageKey("enter-movement-reference-number.scheduled.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-movement-reference-number.error.subsidy-payment-found"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid MRN and user is not the declarant nor consignee but has matching XI eori" in {

        val mrn             = genMRN.sample.get
        val declarantXiEori = genXiEori.sample.get
        val consigneeXiEori = genXiEori.sample.get

        val journey            = session.overpaymentsScheduledJourney.getOrElse(fail("No overpayments journey"))
        val displayDeclaration = buildDisplayDeclaration().withDeclarationId(mrn.value)
        val declarantDetails   = sample[DeclarantDetails].copy(declarantEORI = declarantXiEori.value)
        val consigneeDetails   = sample[ConsigneeDetails].copy(consigneeEORI = consigneeXiEori.value)

        val updatedDisplayResponseDetails = displayDeclaration.displayResponseDetail.copy(
          declarantDetails = declarantDetails,
          consigneeDetails = Some(consigneeDetails)
        )
        val updatedDisplayDeclaration     =
          displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)

        val updatedJourney =
          journey
            .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
            .map(_.submitUserXiEori(UserXiEori(consigneeXiEori.value)))
            .getOrFail

        val updatedSession =
          session.copy(overpaymentsScheduledJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(mrn, Right(Some(updatedDisplayDeclaration)))
          mockGetXiEori(Future.successful(UserXiEori(consigneeXiEori.value)))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(enterMovementReferenceNumberKey -> mrn.value),
          routes.CheckDeclarationDetailsController.show
        )
      }

      "submit a valid MRN and user is not the declarant nor consignee, and has no XI eori" in {

        val mrn             = genMRN.sample.get
        val declarantEori   = genEori.sample.get
        val consigneeXiEori = genXiEori.sample.get

        val journey            = session.overpaymentsScheduledJourney.getOrElse(fail("No overpayments journey"))
        val displayDeclaration = buildDisplayDeclaration().withDeclarationId(mrn.value)
        val declarantDetails   = sample[DeclarantDetails].copy(declarantEORI = declarantEori.value)
        val consigneeDetails   = sample[ConsigneeDetails].copy(consigneeEORI = consigneeXiEori.value)

        val updatedDisplayResponseDetails = displayDeclaration.displayResponseDetail.copy(
          declarantDetails = declarantDetails,
          consigneeDetails = Some(consigneeDetails)
        )
        val updatedDisplayDeclaration     =
          displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)

        val updatedJourney =
          journey
            .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
            .map(_.submitUserXiEori(UserXiEori.NotRegistered))
            .getOrFail

        val updatedSession =
          session.copy(overpaymentsScheduledJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(mrn, Right(Some(updatedDisplayDeclaration)))
          mockGetXiEori(Future.successful(UserXiEori.NotRegistered))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(enterMovementReferenceNumberKey -> mrn.value),
          routes.EnterImporterEoriNumberController.show
        )
      }
    }
  }
}
