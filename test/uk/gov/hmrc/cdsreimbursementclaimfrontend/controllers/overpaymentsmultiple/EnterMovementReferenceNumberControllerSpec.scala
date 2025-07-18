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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen

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

  val session: SessionData = SessionData(emptyJourney)

  private def mockGetDisplayDeclaration(expectedMrn: MRN, response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimsService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(expectedMrn, *)
      .returning(EitherT.fromEither[Future](response))

  @scala.annotation.nowarn
  private def mockGetXiEori(response: Future[UserXiEori]) =
    (mockXiEoriConnector
      .getXiEori(_: HeaderCarrier))
      .expects(*)
      .returning(response)

  val messageKey: String = "enter-movement-reference-number"

  "MRN Controller" when {
    "Enter MRN page" must {

      def performAction(): Future[Result] = controller.showFirst()(FakeRequest())

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messageKey.multiple.title", "First"),
          doc => {
            doc
              .getElementById(s"$messageKey-label")
              .text()                           shouldBe messageFromMessageKey(
              s"$messageKey.multiple.label",
              "first"
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
        val sessionToAmend = SessionData(journey)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messageKey.multiple.title", "First"),
          doc => {
            doc
              .getElementById(s"$messageKey-hint")
              .text()                              shouldBe messageFromMessageKey(
              s"$messageKey.help"
            )
            doc.getElementById(messageKey).`val`() shouldBe mrn.value
          }
        )
      }

      "redirect to check movement reference number when page index is invalid" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(controller.show(0)(FakeRequest()), routes.CheckMovementReferenceNumbersController.show.url)
      }
    }

    "Submit MRN page" must {

      def performAction(data: (String, String)*)(pageIndex: Int = 1): Future[Result] =
        controller.submit(pageIndex)(FakeRequest().withFormUrlEncodedBody(data*))

      val leadMrn            = sample[MRN]
      val secondMrn          = sample[MRN]
      val displayDeclaration = buildDisplayDeclaration()
      val journey            = session.overpaymentsMultipleJourney.get

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

      "reject an empty MRN" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number" -> "")(),
          messageFromMessageKey(s"$messageKey.multiple.title", "First"),
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
          performAction("enter-movement-reference-number" -> invalidMRN.value)(),
          messageFromMessageKey(s"$messageKey.multiple.title", "First"),
          doc => {
            getErrorSummary(doc)                   shouldBe messageFromMessageKey(s"$messageKey.invalid.number")
            doc.getElementById(messageKey).`val`() shouldBe "INVALID_MOVEMENT_REFERENCE_NUMBER"
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an unknown mrn or mrn without declaration" in forAll { (mrn: MRN) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(mrn, Right(None))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> mrn.value)(),
          routes.ProblemWithMrnController.show(1, mrn)
        )
      }

      "redirect to check movement reference number when page index is invalid" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(controller.submit(0)(FakeRequest()), routes.CheckMovementReferenceNumbersController.show.url)
      }

      "redirect to CheckDeclarationDetails page for first MRN" in {

        val updatedJourney =
          journey.submitMovementReferenceNumberAndDeclaration(leadMrn, getDisplayDeclarationForMrn(leadMrn)).getOrFail
        val updatedSession = SessionData(updatedJourney)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(leadMrn, Right(Some(getDisplayDeclarationForMrn(leadMrn))))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> leadMrn.value)(),
          routes.CheckDeclarationDetailsController.show
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

        val updatedSession = SessionData(updatedJourney)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(leadMrn, Right(Some(displayDeclaration)))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> leadMrn.value)(),
          routes.EnterImporterEoriNumberController.show
        )
      }

      // "redirect to Enter Importer Eori page when user eori is not matching declaration XI eori's for first MRN" in {
      //   val displayDeclaration =
      //     getDisplayDeclarationForMrn(leadMrn)
      //       .withDeclarantEori(anotherExampleXIEori)
      //       .withConsigneeEori(yetAnotherExampleXIEori)

      //   val updatedJourney =
      //     journey
      //       .submitMovementReferenceNumberAndDeclaration(leadMrn, displayDeclaration)
      //       .map(_.submitUserXiEori(UserXiEori.NotRegistered))
      //       .getOrFail

      //   val updatedSession = SessionData(updatedJourney)

      //   inSequence {
      //     mockAuthWithDefaultRetrievals()
      //     mockGetSession(session)
      //     mockGetDisplayDeclaration(leadMrn, Right(Some(displayDeclaration)))
      //     mockGetXiEori(Future.successful(UserXiEori.NotRegistered))
      //     mockStoreSession(updatedSession)(Right(()))
      //   }

      //   checkIsRedirect(
      //     performAction("enter-movement-reference-number" -> leadMrn.value)(),
      //     routes.EnterImporterEoriNumberController.show
      //   )
      // }

      // "redirect to CheckDeclarationDetails page for first MRN if user's XI eori matches declaration eori's" in {
      //   val displayDeclaration =
      //     getDisplayDeclarationForMrn(leadMrn)
      //       .withDeclarantEori(exampleXIEori)
      //       .withConsigneeEori(anotherExampleXIEori)

      //   val updatedJourney =
      //     journey
      //       .submitMovementReferenceNumberAndDeclaration(leadMrn, displayDeclaration)
      //       .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value)))
      //       .getOrFail

      //   val updatedSession = SessionData(updatedJourney)

      //   inSequence {
      //     mockAuthWithDefaultRetrievals()
      //     mockGetSession(session)
      //     mockGetDisplayDeclaration(leadMrn, Right(Some(displayDeclaration)))
      //     mockGetXiEori(Future.successful(UserXiEori(exampleXIEori.value)))
      //     mockStoreSession(updatedSession)(Right(()))
      //   }

      //   checkIsRedirect(
      //     performAction("enter-movement-reference-number" -> leadMrn.value)(),
      //     routes.CheckDeclarationDetailsController.show
      //   )
      // }

      "redirect to Check Movement Reference Numbers page for second MRN when declarantEORI matches" in {
        val updatedJourneyWithLeadMrn   = journey
          .submitMovementReferenceNumberAndDeclaration(leadMrn, getDisplayDeclarationForMrn(leadMrn))
          .getOrFail
        val updatedJourneyWithSecondMrn = updatedJourneyWithLeadMrn
          .submitMovementReferenceNumberAndDeclaration(1, secondMrn, getDisplayDeclarationForMrn(secondMrn))
          .getOrFail

        val updatedSessionWithLeadMrn   = SessionData(updatedJourneyWithLeadMrn)
        val updatedSessionWithSecondMrn = SessionData(updatedJourneyWithSecondMrn)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSessionWithLeadMrn)
          mockGetDisplayDeclaration(secondMrn, Right(Some(getDisplayDeclarationForMrn(secondMrn))))
          mockStoreSession(updatedSessionWithSecondMrn)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> secondMrn.value)(2),
          routes.CheckMovementReferenceNumbersController.show
        )
      }

      "redirect to the Select duties page when amending the non-first MRN" in forAll(
        completeJourneyGen,
        Acc14Gen.arbitraryDisplayDeclaration.arbitrary
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
          val updatedSession = SessionData(updatedJourney)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockGetDisplayDeclaration(correctedDD.getMRN, Right(Some(correctedDD)))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("enter-movement-reference-number" -> correctedDD.getMRN.value)(mrnToChange),
            routes.SelectDutiesController.show(mrnToChange)
          )
        }
      }

      "reject a lead MRN with subsidies payment method" in forAll { (mrn: MRN, declarant: Eori, consignee: Eori) =>
        val session = SessionData.empty.copy(
          overpaymentsMultipleJourney = Some(
            OverpaymentsMultipleJourney
              .empty(
                exampleEori,
                features = Some(
                  OverpaymentsMultipleJourney
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
          performAction("enter-movement-reference-number" -> mrn.value)(),
          messageFromMessageKey("enter-movement-reference-number.multiple.title", "First"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-movement-reference-number.error.subsidy-payment-found"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject a non-first MRN with subsidies payment method" in forAll(
        journeyWithMrnAndDeclarationWithFeatures(
          OverpaymentsMultipleJourney.Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = false)
        ),
        genMRN
      ) { (journey, mrn: MRN) =>
        val displayDeclaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false), (TaxCode.A70, 100, false)))
            .withDeclarationId(mrn.value)
            .withDeclarantEori(journey.getDeclarantEoriFromACC14.value)
            .withConsigneeEori(journey.getConsigneeEoriFromACC14.value)
            .withSomeSubsidiesPaymentMethod()

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockGetDisplayDeclaration(mrn, Right(Some(displayDeclaration)))
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number" -> mrn.value)(2),
          messageFromMessageKey("enter-movement-reference-number.multiple.title", "Second"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-movement-reference-number.error.subsidy-payment-found"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject MRN when EORI of lead display declaration doesn't match declaration EORI" in {
        val updatedJourneyWithLeadMrn = journey
          .submitMovementReferenceNumberAndDeclaration(leadMrn, getDisplayDeclarationForMrn(leadMrn))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedJourneyWithLeadMrn))
          mockGetDisplayDeclaration(
            secondMrn,
            Right(Some(getDisplayDeclarationForMrn(secondMrn, Some(anotherExampleEori))))
          )
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number" -> secondMrn.value)(2),
          messageFromMessageKey("enter-movement-reference-number.multiple.title", "Second"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-movement-reference-number.multiple.error.wrongMRN"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject MRN and require subsidy when payment methods are mixed in the declaration being submitted and lead declaration contains subsidy" in {
        val updatedJourney = OverpaymentsMultipleJourney
          .tryBuildFrom(
            journey.answers,
            Some(
              OverpaymentsMultipleJourney.Features(shouldBlockSubsidies = false, shouldAllowSubsidyOnlyPayments = true)
            )
          )
          .getOrElse(fail("Failed to update session"))

        val displayDeclaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false), (TaxCode.A70, 100, false)))
            .withDeclarationId(leadMrn.value)
            .withDeclarantEori(updatedJourney.answers.userEoriNumber)
            .withSomeSubsidiesPaymentMethod()

        val updatedJourneyWithLeadMrn = updatedJourney
          .submitMovementReferenceNumberAndDeclaration(
            leadMrn,
            displayDeclaration
          )
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedJourneyWithLeadMrn))
          mockGetDisplayDeclaration(
            secondMrn,
            Right(Some(getDisplayDeclarationForMrn(secondMrn).withSomeSubsidiesPaymentMethod()))
          )
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number" -> secondMrn.value)(2),
          messageFromMessageKey("enter-movement-reference-number.multiple.title", "Second"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-movement-reference-number.error.needsSubsidy"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject MRN and require non subsidy when payment methods are mixed in the declaration being submitted and lead declaration doesn't contain subsidy" in {
        val updatedJourney = OverpaymentsMultipleJourney
          .tryBuildFrom(
            journey.answers,
            Some(
              OverpaymentsMultipleJourney.Features(shouldBlockSubsidies = false, shouldAllowSubsidyOnlyPayments = true)
            )
          )
          .getOrElse(fail("Failed to update session"))

        val displayDeclaration =
          buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false), (TaxCode.A70, 100, false)))
            .withDeclarationId(leadMrn.value)
            .withDeclarantEori(updatedJourney.answers.userEoriNumber)

        val updatedJourneyWithLeadMrn = updatedJourney
          .submitMovementReferenceNumberAndDeclaration(
            leadMrn,
            displayDeclaration
          )
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedJourneyWithLeadMrn))
          mockGetDisplayDeclaration(
            secondMrn,
            Right(Some(getDisplayDeclarationForMrn(secondMrn).withSomeSubsidiesPaymentMethod()))
          )
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number" -> secondMrn.value)(2),
          messageFromMessageKey("enter-movement-reference-number.multiple.title", "Second"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-movement-reference-number.error.needsNonSubsidy"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject MRN when the same MRN already exists at a different index" in {
        val updatedJourneyWithLeadMrn = journey
          .submitMovementReferenceNumberAndDeclaration(leadMrn, getDisplayDeclarationForMrn(leadMrn))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedJourneyWithLeadMrn))
          mockGetDisplayDeclaration(leadMrn, Right(Some(getDisplayDeclarationForMrn(leadMrn))))
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number" -> leadMrn.value)(2),
          messageFromMessageKey("enter-movement-reference-number.multiple.title", "Second"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-movement-reference-number.multiple.error.existingMRN"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "redirect to problem with declaration if there are unsupported tax codes" in {
        val ndrcDetails                = NdrcDetails("999", "1200", "payment-method", "payment-reference", None)
        val displayResponseDetail      =
          getDisplayDeclarationForMrn(leadMrn).displayResponseDetail.copy(ndrcDetails = Some(List(ndrcDetails)))
        val displayResponseDeclaration =
          getDisplayDeclarationForMrn(leadMrn).copy(displayResponseDetail = displayResponseDetail)

        val updatedJourneyWithLeadMrn = journey
          .submitMovementReferenceNumberAndDeclaration(leadMrn, displayResponseDeclaration)
          .getOrFail

        val updatedSessionWithLeadMrn = SessionData(updatedJourneyWithLeadMrn)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockGetDisplayDeclaration(leadMrn, Right(Some(displayResponseDeclaration)))
          mockStoreSession(updatedSessionWithLeadMrn)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> leadMrn.value)(1),
          routes.ProblemWithDeclarationController.show
        )
      }

      "redirect to problem with declaration if there are unsupported tax codes for 2nd MRN" in {
        val ndrcDetails                = NdrcDetails("999", "1200", "payment-method", "payment-reference", None)
        val displayResponseDetail      =
          getDisplayDeclarationForMrn(secondMrn).displayResponseDetail.copy(ndrcDetails = Some(List(ndrcDetails)))
        val displayResponseDeclaration =
          getDisplayDeclarationForMrn(secondMrn).copy(displayResponseDetail = displayResponseDetail)

        val updatedJourneyWithLeadMrn   = journey
          .submitMovementReferenceNumberAndDeclaration(leadMrn, getDisplayDeclarationForMrn(leadMrn))
          .getOrFail
        val updatedJourneyWithSecondMrn = updatedJourneyWithLeadMrn
          .submitMovementReferenceNumberAndDeclaration(1, secondMrn, displayResponseDeclaration)
          .getOrFail

        val updatedSessionWithLeadMrn   = SessionData(updatedJourneyWithLeadMrn)
        val updatedSessionWithSecondMrn = SessionData(updatedJourneyWithSecondMrn)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSessionWithLeadMrn)
          mockGetDisplayDeclaration(secondMrn, Right(Some(displayResponseDeclaration)))
          mockStoreSession(updatedSessionWithSecondMrn)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> secondMrn.value)(2),
          routes.ProblemWithDeclarationController.showNth(2)
        )
      }
    }
  }
}
