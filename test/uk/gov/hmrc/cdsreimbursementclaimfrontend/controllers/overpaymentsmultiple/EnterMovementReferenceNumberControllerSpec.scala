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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
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

  val session: SessionData = SessionData(emptyClaim)

  private def mockGetImportDeclaration(expectedMrn: MRN, response: Either[Error, Option[ImportDeclaration]]) =
    (mockClaimsService
      .getImportDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(expectedMrn, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockGetXiEori(response: Future[UserXiEori]) =
    (mockXiEoriConnector
      .getXiEori(_: HeaderCarrier))
      .expects(*)
      .returning(response)

  val messageKey: String = "enter-movement-reference-number"

  "MRN Controller" when {
    "Enter MRN page" must {

      def performAction(): Future[Result] = controller.showFirst()(FakeRequest())

      "display the page on a new claim" in {
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

      "display the page on a pre-existing claim" in forAll(
        buildCompleteClaimGen()
      ) { claim =>
        val mrn            = claim.getLeadMovementReferenceNumber.get
        val sessionToAmend = SessionData(claim)

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

      val leadMrn           = sample[MRN]
      val secondMrn         = sample[MRN]
      val importDeclaration = buildImportDeclaration()
      val claim             = session.overpaymentsMultipleClaim.get

      def getImportDeclarationForMrn(mrn: MRN, declarantEori: Option[Eori] = None) =
        importDeclaration
          .copy(displayResponseDetail =
            importDeclaration.displayResponseDetail
              .copy(
                declarantDetails = importDeclaration.displayResponseDetail.declarantDetails
                  .copy(declarantEORI = declarantEori.getOrElse(claim.answers.userEoriNumber).value),
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

      "reject an unknown mrn or mrn without declaration" in forAll { (mrn: MRN) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetImportDeclaration(mrn, Right(None))
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

        val updatedClaim   =
          claim.submitMovementReferenceNumberAndDeclaration(leadMrn, getImportDeclarationForMrn(leadMrn)).getOrFail
        val updatedSession = SessionData(updatedClaim)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetImportDeclaration(leadMrn, Right(Some(getImportDeclarationForMrn(leadMrn))))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> leadMrn.value)(),
          routes.CheckDeclarationDetailsController.show
        )
      }

      "redirect to Enter Importer Eori page when user eori is not matching declaration GB eori's for first MRN" in {
        val importDeclaration =
          getImportDeclarationForMrn(leadMrn)
            .withDeclarantEori(anotherExampleEori)
            .withConsigneeEori(yetAnotherExampleEori)

        val updatedClaim =
          claim
            .submitMovementReferenceNumberAndDeclaration(leadMrn, importDeclaration)
            .getOrFail

        val updatedSession = SessionData(updatedClaim)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetImportDeclaration(leadMrn, Right(Some(importDeclaration)))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> leadMrn.value)(),
          routes.EnterImporterEoriNumberController.show
        )
      }

      "redirect to Enter Importer Eori page when user eori is not matching declaration XI eori's for first MRN" ignore {
        val importDeclaration =
          getImportDeclarationForMrn(leadMrn)
            .withDeclarantEori(anotherExampleXIEori)
            .withConsigneeEori(yetAnotherExampleXIEori)

        val updatedClaim =
          claim
            .submitMovementReferenceNumberAndDeclaration(leadMrn, importDeclaration)
            .map(_.submitUserXiEori(UserXiEori.NotRegistered))
            .getOrFail

        val updatedSession = SessionData(updatedClaim)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetImportDeclaration(leadMrn, Right(Some(importDeclaration)))
          mockGetXiEori(Future.successful(UserXiEori.NotRegistered))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> leadMrn.value)(),
          routes.EnterImporterEoriNumberController.show
        )
      }

      "redirect to CheckDeclarationDetails page for first MRN if user's XI eori matches declaration eori's" ignore {
        val importDeclaration =
          getImportDeclarationForMrn(leadMrn)
            .withDeclarantEori(exampleXIEori)
            .withConsigneeEori(anotherExampleXIEori)

        val updatedClaim =
          claim
            .submitMovementReferenceNumberAndDeclaration(leadMrn, importDeclaration)
            .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value)))
            .getOrFail

        val updatedSession = SessionData(updatedClaim)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetImportDeclaration(leadMrn, Right(Some(importDeclaration)))
          mockGetXiEori(Future.successful(UserXiEori(exampleXIEori.value)))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> leadMrn.value)(),
          routes.CheckDeclarationDetailsController.show
        )
      }

      "redirect to Check Movement Reference Numbers page for second MRN when declarantEORI matches" in {
        val updatedClaimWithLeadMrn   = claim
          .submitMovementReferenceNumberAndDeclaration(leadMrn, getImportDeclarationForMrn(leadMrn))
          .getOrFail
        val updatedClaimWithSecondMrn = updatedClaimWithLeadMrn
          .submitMovementReferenceNumberAndDeclaration(1, secondMrn, getImportDeclarationForMrn(secondMrn))
          .getOrFail

        val updatedSessionWithLeadMrn   = SessionData(updatedClaimWithLeadMrn)
        val updatedSessionWithSecondMrn = SessionData(updatedClaimWithSecondMrn)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSessionWithLeadMrn)
          mockGetImportDeclaration(secondMrn, Right(Some(getImportDeclarationForMrn(secondMrn))))
          mockStoreSession(updatedSessionWithSecondMrn)(Right(()))
        }

        checkIsRedirect(
          performAction("enter-movement-reference-number" -> secondMrn.value)(2),
          routes.CheckMovementReferenceNumbersController.show
        )
      }

      "redirect to the Select duties page when amending the non-first MRN" in forAll(
        completeClaimGen,
        Acc14Gen.arbitraryImportDeclaration.arbitrary
      ) { (claim, newImportDeclaration) =>
        whenever(
          claim
            .getImportDeclarationFor(newImportDeclaration.getMRN)
            .isEmpty && claim.countOfMovementReferenceNumbers > 1
        ) {
          val correctedDD = newImportDeclaration
            .copy(displayResponseDetail =
              newImportDeclaration.displayResponseDetail
                .copy(
                  declarantDetails = claim.getLeadImportDeclaration.get.getDeclarantDetails,
                  consigneeDetails = claim.getLeadImportDeclaration.get.getConsigneeDetails
                )
            )

          val mrnToChange = Gen.choose(2, claim.countOfMovementReferenceNumbers).sample.get

          val updatedClaim   =
            claim
              .submitMovementReferenceNumberAndDeclaration(mrnToChange - 1, correctedDD.getMRN, correctedDD)
              .getOrFail
          val updatedSession = SessionData(updatedClaim)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockGetImportDeclaration(correctedDD.getMRN, Right(Some(correctedDD)))
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
          overpaymentsMultipleClaim = Some(
            OverpaymentsMultipleClaim
              .empty(
                exampleEori
              )
          )
        )

        val importDeclaration =
          buildImportDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false), (TaxCode.A70, 100, false)))
            .withDeclarationId(mrn.value)
            .withDeclarantEori(declarant)
            .withConsigneeEori(consignee)
            .withSomeSubsidiesPaymentMethod()

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockGetImportDeclaration(mrn, Right(Some(importDeclaration)))
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number" -> mrn.value)(),
          messageFromMessageKey("enter-movement-reference-number.multiple.title", "First"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-movement-reference-number.error.has-only-subsidy-items"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject a non-first MRN with subsidies payment method" in forAll(
        claimWithMrnAndDeclarationWithFeatures(
          OverpaymentsMultipleClaim.Features()
        ),
        genMRN
      ) { (claim, mrn: MRN) =>
        val importDeclaration =
          buildImportDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false), (TaxCode.A70, 100, false)))
            .withDeclarationId(mrn.value)
            .withDeclarantEori(claim.getDeclarantEoriFromACC14.value)
            .withConsigneeEori(claim.getConsigneeEoriFromACC14.value)
            .withSomeSubsidiesPaymentMethod()

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockGetImportDeclaration(mrn, Right(Some(importDeclaration)))
        }

        checkPageIsDisplayed(
          performAction("enter-movement-reference-number" -> mrn.value)(2),
          messageFromMessageKey("enter-movement-reference-number.multiple.title", "Second"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-movement-reference-number.error.has-only-subsidy-items"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject MRN when EORI of lead display declaration doesn't match declaration EORI" in {
        val updatedClaimWithLeadMrn = claim
          .submitMovementReferenceNumberAndDeclaration(leadMrn, getImportDeclarationForMrn(leadMrn))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaimWithLeadMrn))
          mockGetImportDeclaration(
            secondMrn,
            Right(Some(getImportDeclarationForMrn(secondMrn, Some(anotherExampleEori))))
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

      "reject MRN when the same MRN already exists at a different index" in {
        val updatedClaimWithLeadMrn = claim
          .submitMovementReferenceNumberAndDeclaration(leadMrn, getImportDeclarationForMrn(leadMrn))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaimWithLeadMrn))
          mockGetImportDeclaration(leadMrn, Right(Some(getImportDeclarationForMrn(leadMrn))))
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
          getImportDeclarationForMrn(leadMrn).displayResponseDetail.copy(ndrcDetails = Some(List(ndrcDetails)))
        val displayResponseDeclaration =
          getImportDeclarationForMrn(leadMrn).copy(displayResponseDetail = displayResponseDetail)

        val updatedClaimWithLeadMrn = claim
          .submitMovementReferenceNumberAndDeclaration(leadMrn, displayResponseDeclaration)
          .getOrFail

        val updatedSessionWithLeadMrn = SessionData(updatedClaimWithLeadMrn)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockGetImportDeclaration(leadMrn, Right(Some(displayResponseDeclaration)))
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
          getImportDeclarationForMrn(secondMrn).displayResponseDetail.copy(ndrcDetails = Some(List(ndrcDetails)))
        val displayResponseDeclaration =
          getImportDeclarationForMrn(secondMrn).copy(displayResponseDetail = displayResponseDetail)

        val updatedClaimWithLeadMrn   = claim
          .submitMovementReferenceNumberAndDeclaration(leadMrn, getImportDeclarationForMrn(leadMrn))
          .getOrFail
        val updatedClaimWithSecondMrn = updatedClaimWithLeadMrn
          .submitMovementReferenceNumberAndDeclaration(1, secondMrn, displayResponseDeclaration)
          .getOrFail

        val updatedSessionWithLeadMrn   = SessionData(updatedClaimWithLeadMrn)
        val updatedSessionWithSecondMrn = SessionData(updatedClaimWithSecondMrn)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSessionWithLeadMrn)
          mockGetImportDeclaration(secondMrn, Right(Some(displayResponseDeclaration)))
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
