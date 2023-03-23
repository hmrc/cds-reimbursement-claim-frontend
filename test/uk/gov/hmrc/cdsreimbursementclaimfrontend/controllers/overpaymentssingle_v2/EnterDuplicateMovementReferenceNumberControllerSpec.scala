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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2

import cats.data.EitherT
import cats.implicits._
import org.scalacheck.Gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarantDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterDuplicateMovementReferenceNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val mockClaimService: ClaimService = mock[ClaimService]

  val enterDuplicateMovementReferenceNumberKey: String =
    "enter-duplicate-movement-reference-number"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  val controller: EnterDuplicateMovementReferenceNumberController =
    instanceOf[EnterDuplicateMovementReferenceNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  private def mockGetDisplayDeclaration(expectedMrn: MRN, response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(expectedMrn, *)
      .returning(EitherT.fromEither[Future](response))

  val journeyGen: Gen[OverpaymentsSingleJourney] =
    buildJourneyGen(answersUpToBasisForClaimGen())
      .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))

  "Duplicate Movement Reference Number Controller" when {
    "Enter MRN page" must {

      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if duplicate declaration is required" in {
        val journey: OverpaymentsSingleJourney =
          journeyGen.sample.get

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-duplicate-movement-reference-number.title"),
          doc => {
            doc.select("#enter-duplicate-movement-reference-number").`val`() shouldBe ""
            doc.select("form").attr("action")                                shouldBe routes.EnterDuplicateMovementReferenceNumberController.submit.url
          }
        )
      }

      "display the page back with MRN populated" in {
        val journey: OverpaymentsSingleJourney =
          journeyGen.sample.get
            .submitDuplicateMovementReferenceNumberAndDeclaration(
              anotherExampleMrn,
              buildDisplayDeclaration(id = anotherExampleMrn.value)
            )
            .getOrFail

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-duplicate-movement-reference-number.title"),
          doc => {
            doc.select("#enter-duplicate-movement-reference-number").`val`() shouldBe anotherExampleMrn.value
            doc.select("form").attr("action")                                shouldBe routes.EnterDuplicateMovementReferenceNumberController.submit.url
          }
        )
      }

      "redirect if duplicate declaration not required" in {
        val journey: OverpaymentsSingleJourney =
          journeyGen.sample.get
            .submitBasisOfClaim(BasisOfOverpaymentClaim.DutySuspension)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterAdditionalDetailsController.show
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

        val journey: OverpaymentsSingleJourney =
          journeyGen.sample.get

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(enterDuplicateMovementReferenceNumberKey -> invalidMRN.value),
          messageFromMessageKey("enter-duplicate-movement-reference-number.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-duplicate-movement-reference-number.invalid.number"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an empty MRN" in {
        val journey: OverpaymentsSingleJourney =
          journeyGen.sample.get

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(enterDuplicateMovementReferenceNumberKey -> ""),
          messageFromMessageKey("enter-duplicate-movement-reference-number.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-duplicate-movement-reference-number.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "submit an unknown MRN" in forAll(journeyGen, genMRN) { case (journey, mrn) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockGetDisplayDeclaration(mrn, Right(None))
        }

        checkIsRedirect(
          performAction(enterDuplicateMovementReferenceNumberKey -> mrn.value),
          baseRoutes.IneligibleController.ineligible()
        )
      }

      "submit a valid MRN and user is declarant" in forAll(journeyGen, genMRN) { case (journey, mrn) =>
        val displayDeclaration            = sample[DisplayDeclaration].withDeclarationId(mrn.value)
        val updatedDeclarantDetails       = displayDeclaration.displayResponseDetail.declarantDetails.copy(
          declarantEORI = journey.answers.userEoriNumber.value
        )
        val updatedDisplayResponseDetails =
          displayDeclaration.displayResponseDetail.copy(declarantDetails = updatedDeclarantDetails)
        val updatedDisplayDeclaration     = displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
        val updatedJourney                =
          journey
            .submitDuplicateMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
            .getOrFail

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockGetDisplayDeclaration(mrn, Right(Some(updatedDisplayDeclaration)))
          mockStoreSession(SessionData(updatedJourney))(Right(()))
        }

        checkIsRedirect(
          performAction(enterDuplicateMovementReferenceNumberKey -> mrn.value),
          routes.CheckDuplicateDeclarationDetailsController.show
        )
      }

      "submit a valid MRN and user is not declarant nor consignee" in forAll(journeyGen, genMRN, genEori, genEori) {
        case (journey, mrn, declarant, consignee) =>
          whenever(declarant =!= exampleEori && consignee =!= exampleEori) {
            val displayDeclaration = sample[DisplayDeclaration].withDeclarationId(mrn.value)
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
                .submitDuplicateMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
                .getOrFail

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
              mockGetDisplayDeclaration(mrn, Right(Some(updatedDisplayDeclaration)))
              mockStoreSession(SessionData(updatedJourney))(Right(()))
            }

            checkIsRedirect(
              performAction(enterDuplicateMovementReferenceNumberKey -> mrn.value),
              routes.EnterImporterEoriNumberOfDuplicateDeclaration.show
            )
          }
      }
    }
  }
}
