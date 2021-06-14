/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import cats.implicits._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.EntryNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, CustomsDataStoreService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterDuplicateMovementReferenceNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with OptionValues {

  val mockCustomsDataStoreService = mock[CustomsDataStoreService]
  val mockClaimsService           = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[CustomsDataStoreService].toInstance(mockCustomsDataStoreService),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  def mockGetDisplayDeclaration(response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimsService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))

  lazy val controller: EnterDuplicateMovementReferenceNumberController =
    instanceOf[EnterDuplicateMovementReferenceNumberController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def getGlobalErrors(doc: Document): Elements = doc.getElementsByClass("govuk-error-summary__list").select("li")

  private def sessionWithClaimState(
    maybeMovementReferenceNumberAnswer: Option[MovementReferenceNumber]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(movementReferenceNumber = maybeMovementReferenceNumberAnswer)
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftC285Claim
    )
  }

  "Movement Reference Number Controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        def performAction(): Future[Result] = controller.enterDuplicateMrn()(FakeRequest())

        val (session, _, _) = sessionWithClaimState(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.StartController.start()
        )

      }
    }

    "Duplicate MRN page" must {
      def performAction(): Future[Result] = controller.enterDuplicateMrn()(FakeRequest())

      "show the title" in {
        val mrn             = MRN("10ABCDEFGHIJKLMNO0")
        val answers         = MovementReferenceNumber(Right(mrn))
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val doc = Jsoup.parse(contentAsString(performAction()))

        doc.select("h1").text should include(messageFromMessageKey("enter-duplicate-movement-reference-number.title"))

      }

      "Fail if the same Entry Number is submitted" in {
        val entryNumber     = EntryNumber("123456789A12345678")
        val answers         = MovementReferenceNumber(Left(entryNumber))
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody("enter-movement-reference-number" -> "123456789A12345678")
        )

        val doc   = Jsoup.parse(contentAsString(result))
        val error = getGlobalErrors(doc).text()
        error shouldBe messageFromMessageKey("enter-movement-reference-number.invalid.enter-different-entry-number")

        status(result) shouldBe BAD_REQUEST
      }

      "Fail on an Entry Number journey and an MRN is submitted" in {
        val entryNumber     = EntryNumber("123456789A12345678")
        val answers         = MovementReferenceNumber(Left(entryNumber))
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody("enter-movement-reference-number" -> "10ABCDEFGHIJKLMNO0")
        )
        val doc             = Jsoup.parse(contentAsString(result))
        val error           = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey("enter-movement-reference-number.invalid.entry-number-not-mrn")
        status(result) shouldBe BAD_REQUEST
      }

      "Fail if the same MRN is submitted" in {
        val mrn             = MRN("10ABCDEFGHIJKLMNO0")
        val answers         = MovementReferenceNumber(Right(mrn))
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody("enter-movement-reference-number" -> "10ABCDEFGHIJKLMNO0")
        )
        val doc             = Jsoup.parse(contentAsString(result))
        val error           = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey("enter-movement-reference-number.invalid.enter-different-mrn")
        status(result) shouldBe BAD_REQUEST
      }

      "Fail on an MRN journey and an entry number is submitted" in {
        val mrn             = MRN("10ABCDEFGHIJKLMNO0")
        val answers         = MovementReferenceNumber(Right(mrn))
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody("enter-movement-reference-number" -> "123456789A12345678")
        )
        val doc             = Jsoup.parse(contentAsString(result))
        val error           = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey("enter-movement-reference-number.invalid.mrn-not-entry-number")
        status(result) shouldBe BAD_REQUEST
      }

      "Redirect to the enter duplicate declaration details page if a different Entry Number is submitted" in {
        val entryNumber     = EntryNumber("123456789A12345678")
        val answers         = MovementReferenceNumber(Left(entryNumber))
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }
        val result          = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody("enter-movement-reference-number" -> "123456789A12341111")
        )
        status(result) shouldBe 303
        redirectLocation(
          result
        ).value shouldBe routes.EnterDeclarationDetailsController.enterDuplicateDeclarationDetails().url
      }

      "Redirect to the enter importer eori page if a different MRN Number is submitted" in {
        val mrn                = MRN("10AAAAAAAAAAAAAAA1")
        val answers            = MovementReferenceNumber(Right(mrn))
        val (session, _, _)    = sessionWithClaimState(Some(answers))
        val displayDeclaration = sample[DisplayDeclaration]
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }
        val result             = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody("enter-movement-reference-number" -> "20AAAAAAAAAAAAAAA1")
        )
        status(result) shouldBe 303
        redirectLocation(
          result
        ).value shouldBe routes.EnterImporterEoriNumberController.enterImporterEoriNumber().url
      }

    }

    "Form validation" must {
      val featureSwitch = instanceOf[FeatureSwitchService]
      val form          = EnterMovementReferenceNumberController.movementReferenceNumberForm(featureSwitch)
      val mrnKey        = "enter-movement-reference-number"

      featureSwitch.EntryNumber.enable()

      "accept valid MRN" in {
        val errors = form.bind(Map(mrnKey -> "10ABCDEFGHIJKLMNO0")).errors
        errors shouldBe Nil
      }

      "accept valid Entry Number (Chief Number)" in {
        val errors = form.bind(Map(mrnKey -> "123456789A12345678")).errors
        errors shouldBe Nil
      }

      "reject 19 characters" in {
        val errors = form.bind(Map(mrnKey -> "910ABCDEFGHIJKLMNO0")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.number")
      }

      "reject empty MRN field" in {
        val errors = form.bind(Map(mrnKey -> " ")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.required")
      }

      "reject 17 characters" in {
        val errors = form.bind(Map(mrnKey -> "123456789A1234567")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.number")
      }
    }

  }
}
