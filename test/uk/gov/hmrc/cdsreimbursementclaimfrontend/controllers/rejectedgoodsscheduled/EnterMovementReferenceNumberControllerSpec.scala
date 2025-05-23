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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import cats.data.EitherT
import cats.implicits.*
import org.scalatest.BeforeAndAfterEach
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarantDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
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
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with DeclarationSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with OptionValues {

  val mockClaimService: ClaimService       = mock[ClaimService]
  val mockXiEoriConnector: XiEoriConnector = mock[XiEoriConnector]

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

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.RejectedGoods)
    ()
  }

  val session: SessionData = SessionData.empty.copy(
    rejectedGoodsScheduledJourney = Some(
      RejectedGoodsScheduledJourney
        .empty(exampleEori)
    )
  )

  val messageKey: String = "enter-movement-reference-number"

  private def mockGetDisplayDeclaration(expectedMrn: MRN, response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(expectedMrn, *)
      .returning(EitherT.fromEither[Future](response))

  @scala.annotation.nowarn
  private def mockGetXiEori(response: Future[UserXiEori]) =
    (mockXiEoriConnector
      .getXiEori(_: HeaderCarrier))
      .expects(*)
      .returning(response)

  "MRN Controller" when {
    "Enter MRN page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"enter-movement-reference-number.scheduled.title"),
          doc => {
            doc
              .getElementById(s"$messageKey-hint")
              .text()                           shouldBe messageFromMessageKey(
              s"$messageKey.help"
            )
            doc.select(s"#$messageKey").`val`() shouldBe ""
            doc.select("form").attr("action")   shouldBe routes.EnterMovementReferenceNumberController.submit.url
          }
        )
      }

      "display the page on a pre-existing journey" in forAll(
        buildCompleteJourneyGen()
      ) { journey =>
        val mrn            = journey.getLeadMovementReferenceNumber.get
        val sessionToAmend = SessionData(journey)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"enter-movement-reference-number.scheduled.title"),
          doc => {
            doc
              .getElementById(s"$messageKey-hint")
              .text()                                                     shouldBe messageFromMessageKey(
              s"$messageKey.help"
            )
            doc.getElementById("enter-movement-reference-number").`val`() shouldBe mrn.value
          }
        )
      }
    }

    "Submit MRN page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty MRN" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number." -> ""),
          messageFromMessageKey(s"enter-movement-reference-number.scheduled.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messageKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid formatted MRN" in {
        val invalidMRN = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number" -> invalidMRN.value),
          messageFromMessageKey(s"enter-movement-reference-number.scheduled.title"),
          doc => {
            getErrorSummary(doc)                                          shouldBe messageFromMessageKey(s"$messageKey.invalid.number")
            doc.getElementById("enter-movement-reference-number").`val`() shouldBe "INVALID_MOVEMENT_REFERENCE_NUMBER"
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an unknown mrn or mrn without declaration " in forAll { (mrn: MRN) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(mrn, Right(None))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> mrn.value),
          routes.ProblemWithMrnController.show(mrn)
        )
      }

      "submit a valid MRN and user is declarant" in forAll { (mrn: MRN) =>
        val journey                       = session.rejectedGoodsScheduledJourney.getOrElse(fail("No rejected goods journey"))
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
        val updatedSession                = SessionData(updatedJourney)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(mrn, Right(Some(updatedDisplayDeclaration)))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> mrn.value),
          routes.CheckDeclarationDetailsController.show
        )
      }

      "submit a valid MRN and user is not the declarant or consignee" in forAll {
        (mrn: MRN, declarant: Eori, consignee: Eori) =>
          whenever(declarant =!= exampleEori && consignee =!= exampleEori) {
            val journey            = session.rejectedGoodsScheduledJourney.getOrElse(fail("No rejected goods journey"))
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
            val updatedSession                = SessionData(updatedJourney)

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(session)
              mockGetDisplayDeclaration(mrn, Right(Some(updatedDisplayDeclaration)))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction("enter-movement-reference-number" -> mrn.value),
              routes.EnterImporterEoriNumberController.show
            )
          }
      }

      // "submit a valid MRN and user is not the declarant nor consignee but has matching XI eori" in {

      //   val mrn             = genMRN.sample.get
      //   val declarantXiEori = genXiEori.sample.get
      //   val consigneeXiEori = genXiEori.sample.get

      //   val journey            = session.rejectedGoodsScheduledJourney.getOrElse(fail("No overpayments journey"))
      //   val displayDeclaration = buildDisplayDeclaration().withDeclarationId(mrn.value)
      //   val declarantDetails   = sample[DeclarantDetails].copy(declarantEORI = declarantXiEori.value)
      //   val consigneeDetails   = sample[ConsigneeDetails].copy(consigneeEORI = consigneeXiEori.value)

      //   val updatedDisplayResponseDetails = displayDeclaration.displayResponseDetail.copy(
      //     declarantDetails = declarantDetails,
      //     consigneeDetails = Some(consigneeDetails)
      //   )
      //   val updatedDisplayDeclaration     =
      //     displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)

      //   val updatedJourney =
      //     journey
      //       .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
      //       .map(_.submitUserXiEori(UserXiEori(consigneeXiEori.value)))
      //       .getOrFail

      //   val updatedSession = SessionData(updatedJourney)

      //   inSequence {
      //     mockAuthWithDefaultRetrievals()
      //     mockGetSession(session)
      //     mockGetDisplayDeclaration(mrn, Right(Some(updatedDisplayDeclaration)))
      //     mockGetXiEori(Future.successful(UserXiEori(consigneeXiEori.value)))
      //     mockStoreSession(updatedSession)(Right(()))
      //   }

      //   checkIsRedirect(
      //     performAction("enter-movement-reference-number" -> mrn.value),
      //     routes.CheckDeclarationDetailsController.show
      //   )
      // }

      // "submit a valid MRN and user is not the declarant nor consignee, and has no XI eori" in {

      //   val mrn             = genMRN.sample.get
      //   val declarantEori   = genEori.sample.get
      //   val consigneeXiEori = genXiEori.sample.get

      //   val journey            = session.rejectedGoodsScheduledJourney.getOrElse(fail("No overpayments journey"))
      //   val displayDeclaration = buildDisplayDeclaration().withDeclarationId(mrn.value)
      //   val declarantDetails   = sample[DeclarantDetails].copy(declarantEORI = declarantEori.value)
      //   val consigneeDetails   = sample[ConsigneeDetails].copy(consigneeEORI = consigneeXiEori.value)

      //   val updatedDisplayResponseDetails = displayDeclaration.displayResponseDetail.copy(
      //     declarantDetails = declarantDetails,
      //     consigneeDetails = Some(consigneeDetails)
      //   )
      //   val updatedDisplayDeclaration     =
      //     displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)

      //   val updatedJourney =
      //     journey
      //       .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
      //       .map(_.submitUserXiEori(UserXiEori.NotRegistered))
      //       .getOrFail

      //   val updatedSession = SessionData(updatedJourney)

      //   inSequence {
      //     mockAuthWithDefaultRetrievals()
      //     mockGetSession(session)
      //     mockGetDisplayDeclaration(mrn, Right(Some(updatedDisplayDeclaration)))
      //     mockGetXiEori(Future.successful(UserXiEori.NotRegistered))
      //     mockStoreSession(updatedSession)(Right(()))
      //   }

      //   checkIsRedirect(
      //     performAction("enter-movement-reference-number" -> mrn.value),
      //     routes.EnterImporterEoriNumberController.show
      //   )
      // }

      "reject an MRN with subsidies payment method" in forAll { (mrn: MRN, declarant: Eori, consignee: Eori) =>
        val session: SessionData = SessionData.empty.copy(
          rejectedGoodsScheduledJourney = Some(
            RejectedGoodsScheduledJourney
              .empty(
                exampleEori,
                features = Some(
                  RejectedGoodsScheduledJourney
                    .Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = false)
                )
              )
          )
        )

        val displayDeclaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false), (TaxCode.A70, 100, false)))
            .withDeclarationId(mrn.value)
            .withDeclarantEori(declarant)
            .withConsigneeEori(consignee)
            .withSomeSubsidiesPaymentMethod()

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(mrn, Right(Some(displayDeclaration)))
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number" -> mrn.value),
          messageFromMessageKey("enter-movement-reference-number.scheduled.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-movement-reference-number.error.subsidy-payment-found"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

    }
  }
}
