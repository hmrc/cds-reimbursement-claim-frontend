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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import cats.data.EitherT
import org.scalacheck.Gen
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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector

class EnterMovementReferenceNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with DeclarationSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with OptionValues {

  val mockClaimsService: ClaimService      = mock[ClaimService]
  val mockXiEoriConnector: XiEoriConnector = mock[XiEoriConnector]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService),
      bind[XiEoriConnector].toInstance(mockXiEoriConnector)
    )

  val controller: EnterMovementReferenceNumberController = instanceOf[EnterMovementReferenceNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData(emptyJourney)

  private def mockGetDisplayDeclaration(expectedMrn: MRN, response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimsService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(expectedMrn, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockGetXiEori(response: Future[UserXiEori]) =
    (mockXiEoriConnector
      .getXiEori(_: HeaderCarrier))
      .expects(*)
      .returning(response)

  val messageKey: String = "enter-movement-reference-number.rejected-goods"

  "MRN Controller" when {
    "Enter MRN page" must {

      def performAction(): Future[Result] = controller.showFirst()(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messageKey.multiple.title", "first"),
          doc => {
            doc
              .getElementById(s"$messageKey-hint")
              .text()                           shouldBe messageFromMessageKey(
              s"$messageKey.multiple.help"
            )
            doc.select(s"#$messageKey").`val`() shouldBe ""
            doc.select("form").attr("action")   shouldBe routes.EnterMovementReferenceNumberController
              .submit(1)
              .url
          }
        )
      }

      "display the page on a pre-existing journey" in forAll(
        buildCompleteJourneyGen()
      ) { journey =>
        val mrn            = journey.getLeadMovementReferenceNumber.get
        val sessionToAmend = session.copy(rejectedGoodsMultipleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messageKey.multiple.title", "first"),
          doc => {
            doc
              .getElementById(s"$messageKey-hint")
              .text()                              shouldBe messageFromMessageKey(
              s"$messageKey.multiple.help"
            )
            doc.getElementById(messageKey).`val`() shouldBe mrn.value
          }
        )
      }
    }

    "Submit MRN page" must {

      def performAction(data: (String, String)*)(pageIndex: Int = 1): Future[Result] =
        controller.submit(pageIndex)(FakeRequest().withFormUrlEncodedBody(data: _*))

      val leadMrn            = sample[MRN]
      val secondMrn          = sample[MRN]
      val displayDeclaration = buildDisplayDeclaration()
      val journey            = session.rejectedGoodsMultipleJourney.get

      def getDisplayDeclarationForMrn(mrn: MRN, declarantEori: Option[Eori] = None) =
        displayDeclaration
          .copy(displayResponseDetail =
            displayDeclaration.displayResponseDetail
              .copy(
                declarantDetails = displayDeclaration.displayResponseDetail.declarantDetails
                  .copy(declarantEORI = declarantEori.getOrElse(journey.answers.userEoriNumber).value),
                declarationId = mrn.value
              )
          )

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()()) shouldBe NOT_FOUND
      }

      "reject an empty MRN" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number.rejected-goods" -> "")(),
          messageFromMessageKey(s"$messageKey.multiple.title", "first"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messageKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid MRN" in {
        val invalidMRN = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number.rejected-goods" -> invalidMRN.value)(),
          messageFromMessageKey(s"$messageKey.multiple.title", "first"),
          doc => {
            getErrorSummary(doc)                   shouldBe messageFromMessageKey(s"$messageKey.invalid.number")
            doc.getElementById(messageKey).`val`() shouldBe "INVALID_MOVEMENT_REFERENCE_NUMBER"
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "redirect to CheckDeclarationDetails page for first MRN" in {

        val updatedJourney =
          journey.submitMovementReferenceNumberAndDeclaration(leadMrn, getDisplayDeclarationForMrn(leadMrn)).getOrFail
        val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(leadMrn, Right(Some(getDisplayDeclarationForMrn(leadMrn))))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number.rejected-goods" -> leadMrn.value)(),
          routes.CheckDeclarationDetailsController.show()
        )
      }

      "redirect to Enter Importer Eori page when user eori is not matching declaration GB eori's for first MRN" in {
        val displayDeclaration =
          getDisplayDeclarationForMrn(leadMrn)
            .withDeclarantEori(anotherExampleEori)
            .withConsigneeEori(yetAnotherExampleEori)

        val updatedJourney =
          journey
            .submitMovementReferenceNumberAndDeclaration(leadMrn, displayDeclaration)
            .getOrFail

        val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(leadMrn, Right(Some(displayDeclaration)))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number.rejected-goods" -> leadMrn.value)(),
          routes.EnterImporterEoriNumberController.show()
        )
      }

      "redirect to Enter Importer Eori page when user eori is not matching declaration XI eori's for first MRN" in {
        val displayDeclaration =
          getDisplayDeclarationForMrn(leadMrn)
            .withDeclarantEori(anotherExampleXIEori)
            .withConsigneeEori(yetAnotherExampleXIEori)

        val updatedJourney =
          journey
            .submitMovementReferenceNumberAndDeclaration(leadMrn, displayDeclaration)
            .map(_.submitUserXiEori(UserXiEori.NotRegistered))
            .getOrFail

        val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(leadMrn, Right(Some(displayDeclaration)))
          mockGetXiEori(Future.successful(UserXiEori.NotRegistered))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number.rejected-goods" -> leadMrn.value)(),
          routes.EnterImporterEoriNumberController.show()
        )
      }

      "redirect to CheckDeclarationDetails page for first MRN if user's XI eori matches declaration eori's" in {
        val displayDeclaration =
          getDisplayDeclarationForMrn(leadMrn)
            .withDeclarantEori(exampleXIEori)
            .withConsigneeEori(anotherExampleXIEori)

        val updatedJourney =
          journey
            .submitMovementReferenceNumberAndDeclaration(leadMrn, displayDeclaration)
            .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value)))
            .getOrFail

        val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(leadMrn, Right(Some(displayDeclaration)))
          mockGetXiEori(Future.successful(UserXiEori(exampleXIEori.value)))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number.rejected-goods" -> leadMrn.value)(),
          routes.CheckDeclarationDetailsController.show()
        )
      }

      "redirect to Check Movement Reference Numbers page for second MRN when declarantEORI matches" in {

        val updatedJourneyWithLeadMrn   = journey
          .submitMovementReferenceNumberAndDeclaration(leadMrn, getDisplayDeclarationForMrn(leadMrn))
          .getOrFail
        val updatedJourneyWithSecondMrn = updatedJourneyWithLeadMrn
          .submitMovementReferenceNumberAndDeclaration(1, secondMrn, getDisplayDeclarationForMrn(secondMrn))
          .getOrFail

        val updatedSessionWithLeadMrn   = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourneyWithLeadMrn))
        val updatedSessionWithSecondMrn = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourneyWithSecondMrn))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSessionWithLeadMrn)
          mockGetDisplayDeclaration(secondMrn, Right(Some(getDisplayDeclarationForMrn(secondMrn))))
          mockStoreSession(updatedSessionWithSecondMrn)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number.rejected-goods" -> secondMrn.value)(2),
          routes.CheckMovementReferenceNumbersController.show()
        )
      }

      "redirect to the Select duties page when amending the non-first MRN" in forAll(
        completeJourneyGen,
        arbitraryDisplayDeclaration.arbitrary
      ) { (journey, newDisplayDeclaration) =>
        whenever(
          journey
            .getDisplayDeclarationFor(newDisplayDeclaration.getMRN)
            .isEmpty && journey.countOfMovementReferenceNumbers > 1
        ) {
          val correctedDD = newDisplayDeclaration
            .copy(displayResponseDetail =
              newDisplayDeclaration.displayResponseDetail
                .copy(
                  declarantDetails = journey.getLeadDisplayDeclaration.get.getDeclarantDetails,
                  consigneeDetails = journey.getLeadDisplayDeclaration.get.getConsigneeDetails
                )
            )

          val mrnToChange = Gen.choose(2, journey.countOfMovementReferenceNumbers).sample.get

          val updatedJourney =
            journey
              .submitMovementReferenceNumberAndDeclaration(mrnToChange - 1, correctedDD.getMRN, correctedDD)
              .getOrFail
          val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(rejectedGoodsMultipleJourney = Some(journey)))
            mockGetDisplayDeclaration(correctedDD.getMRN, Right(Some(correctedDD)))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("enter-movement-reference-number.rejected-goods" -> correctedDD.getMRN.value)(mrnToChange),
            routes.SelectDutiesController.show(mrnToChange)
          )
        }
      }

      "reject a lead MRN with subsidies payment method" in forAll { (mrn: MRN, declarant: Eori, consignee: Eori) =>
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
          performAction(messageKey -> mrn.value)(),
          messageFromMessageKey(s"$messageKey.multiple.title", "first"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messageKey.error.subsidy-payment-found"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject a non-first MRN with subsidies payment method" in forAll(
        journeyWithMrnAndDD,
        genMRN
      ) { (journey, mrn: MRN) =>
        featureSwitch.enable(Feature.BlockSubsidies)

        val displayDeclaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false), (TaxCode.A70, 100, false)))
            .withDeclarationId(mrn.value)
            .withDeclarantEori(journey.getDeclarantEoriFromACC14.value)
            .withConsigneeEori(journey.getConsigneeEoriFromACC14.value)
            .withAllSubsidiesPaymentMethod()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(rejectedGoodsMultipleJourney = Some(journey)))
          mockGetDisplayDeclaration(mrn, Right(Some(displayDeclaration)))
        }

        checkPageIsDisplayed(
          performAction(messageKey -> mrn.value)(2),
          messageFromMessageKey(s"$messageKey.multiple.title", "second"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messageKey.error.subsidy-payment-found"
            ),
          expectedStatus = BAD_REQUEST
        )
      }
    }
  }
}
