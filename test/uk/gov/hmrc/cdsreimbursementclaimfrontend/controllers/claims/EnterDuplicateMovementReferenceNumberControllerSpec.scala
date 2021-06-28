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
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDuplicateMovementReferenceNumberController.{enterDuplicateEntryNumberKey, enterDuplicateMovementReferenceNumberKey, enterNoLegacyDuplicateMrnKey}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{genOtherThan, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.EntryNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, CustomsDataStoreService, FeatureSwitchService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterDuplicateMovementReferenceNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with TableDrivenPropertyChecks
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

  val featureSwitch = instanceOf[FeatureSwitchService]
  val keys          =
    Table("Key", enterNoLegacyDuplicateMrnKey, enterDuplicateMovementReferenceNumberKey, enterDuplicateEntryNumberKey)

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

    "Duplicate Entry Number page" must {
      def performAction(): Future[Result] = controller.enterDuplicateMrn()(FakeRequest())

      "Show the title" in {
        featureSwitch.EntryNumber.enable()
        val (session, _, _) = sessionWithClaimState(sampleEntryNumberAnswer())
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val doc = Jsoup.parse(contentAsString(performAction()))

        doc.select("h1").text should include(messageFromMessageKey(s"enter-duplicate-entry-number.title"))
      }

      "Fail if the same Entry Number is submitted" in {

        val entryNumber     = sample[EntryNumber]
        val answers         = sampleEntryNumberAnswer(entryNumber)
        val (session, _, _) = sessionWithClaimState(answers)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody(enterDuplicateEntryNumberKey -> entryNumber.value)
        )

        val doc   = Jsoup.parse(contentAsString(result))
        val error = getGlobalErrors(doc).text()
        error shouldBe messageFromMessageKey(
          s"$enterDuplicateEntryNumberKey.invalid.enter-different-entry-number"
        )

        status(result) shouldBe BAD_REQUEST
      }

      "Fail on an Entry Number journey and an MRN is submitted" in {

        val (session, _, _) = sessionWithClaimState(sampleEntryNumberAnswer())
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody(enterDuplicateEntryNumberKey -> sample[MRN].value)
        )
        val doc             = Jsoup.parse(contentAsString(result))
        val error           = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey(s"$enterDuplicateEntryNumberKey.invalid.entry-number-not-mrn")
        status(result) shouldBe BAD_REQUEST
      }

      "Redirect to the enter duplicate declaration details page if a different Entry Number is submitted" in {

        val entryNumber     = sample[EntryNumber]
        val answers         = sampleEntryNumberAnswer(entryNumber)
        val (session, _, _) = sessionWithClaimState(answers)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }
        val result          = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody(
            enterDuplicateEntryNumberKey -> genOtherThan(entryNumber).value
          )
        )
        status(result) shouldBe 303
        redirectLocation(
          result
        ).value shouldBe routes.EnterDeclarationDetailsController.enterDuplicateDeclarationDetails().url
      }

    }

    "Duplicate MRN page" must {
      def performAction(): Future[Result] = controller.enterDuplicateMrn()(FakeRequest())

      "Show the title" in {
        featureSwitch.EntryNumber.disable()
        val (session, _, _) = sessionWithClaimState(sampleMrnAnswer())
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val doc = Jsoup.parse(contentAsString(performAction()))

        doc.select("h1").text should include(messageFromMessageKey(s"enter-no-legacy-duplicate-mrn.title"))
      }

      "Fail if the same MRN is submitted" in {

        val mrn             = sample[MRN]
        val answers         = sampleMrnAnswer(mrn)
        val (session, _, _) = sessionWithClaimState(answers)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody(enterNoLegacyDuplicateMrnKey -> mrn.value)
        )
        val doc             = Jsoup.parse(contentAsString(result))
        val error           = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey(s"$enterNoLegacyDuplicateMrnKey.invalid.enter-different-mrn")
        status(result) shouldBe BAD_REQUEST
      }

      "Fail on an MRN journey (feature flag enabled) and an entry number is submitted" in {
        featureSwitch.EntryNumber.enable()
        val (session, _, _) = sessionWithClaimState(sampleMrnAnswer())
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody(enterDuplicateMovementReferenceNumberKey -> sample[EntryNumber].value)
        )
        val doc             = Jsoup.parse(contentAsString(result))
        val error           = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey(s"$enterDuplicateMovementReferenceNumberKey.invalid.mrn-not-entry-number")
        status(result) shouldBe BAD_REQUEST
      }

      "Redirect to the enter importer eori page with the if a different MRN Number is submitted on the" in {
        featureSwitch.EntryNumber.disable()
        val mrn                = sample[MRN]
        val answers            = sampleMrnAnswer(mrn)
        val (session, _, _)    = sessionWithClaimState(answers)
        val displayDeclaration = sample[DisplayDeclaration]
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }
        val result             = controller.enterDuplicateMrnSubmit()(
          FakeRequest().withFormUrlEncodedBody(enterNoLegacyDuplicateMrnKey -> genOtherThan(mrn).value)
        )
        status(result) shouldBe 303
        redirectLocation(
          result
        ).value shouldBe routes.EnterImporterEoriNumberController.enterImporterEoriNumber().url
      }

    }

    "Form validation" must {

      def form(key: String, isEntryNumberEnabled: Boolean) =
        EnterMovementReferenceNumberController.movementReferenceNumberForm(key, isEntryNumberEnabled)

      "accept valid MRN" in forAll(keys) { key =>
        val errors = form(key, isEntryNumberEnabled = false).bind(Map(key -> sample[MRN].value)).errors
        errors shouldBe Nil
      }

      "accept valid Entry Number (Chief Number) when legacy journey is enabled" in {
        val errors = form(enterDuplicateMovementReferenceNumberKey, isEntryNumberEnabled = true)
          .bind(Map(enterDuplicateMovementReferenceNumberKey -> sample[EntryNumber].value))
          .errors
        errors shouldBe Nil
      }

      "reject Entry Number (Chief Number) when legacy journey is disabled" in {
        val errors = form(enterNoLegacyDuplicateMrnKey, isEntryNumberEnabled = false)
          .bind(Map(enterNoLegacyDuplicateMrnKey -> "123456789A12345678"))
          .errors
        errors.headOption.value.messages shouldBe List("invalid.number")
      }

      "reject 19 characters" in forAll(keys) { key =>
        val errors = form(key, isEntryNumberEnabled = false).bind(Map(key -> "910ABCDEFGHIJKLMNO0")).errors
        errors.headOption.value.messages shouldBe List("invalid.number")
      }

      "reject empty MRN field" in forAll(keys) { key =>
        val errors = form(key, isEntryNumberEnabled = true).bind(Map(key -> " ")).errors
        errors.headOption.value.messages shouldBe List("error.required")
      }

      "reject 17 characters" in {
        val errors = form(enterNoLegacyDuplicateMrnKey, isEntryNumberEnabled = true)
          .bind(Map(enterNoLegacyDuplicateMrnKey -> "123456789A1234567"))
          .errors
        errors.headOption.value.messages shouldBe List("invalid.number")
      }
    }

  }
}
