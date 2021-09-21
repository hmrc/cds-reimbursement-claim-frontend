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
import cats.{Functor, Id}
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDuplicateMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{ConsigneeDetails, DisplayDeclaration}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{genOtherThan, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, CustomsDataStoreService}
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

  val testCases = Table(
    ("NumberOfClaimsType", "JourneyBindable"),
    (SelectNumberOfClaimsAnswer.Individual, JourneyBindable.Single),
    (SelectNumberOfClaimsAnswer.Multiple, JourneyBindable.Multiple),
    (SelectNumberOfClaimsAnswer.Scheduled, JourneyBindable.Scheduled)
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
    maybeMovementReferenceNumberAnswer: Option[MovementReferenceNumber],
    numberOfClaims: Option[SelectNumberOfClaimsAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(
        movementReferenceNumber = maybeMovementReferenceNumberAnswer,
        selectNumberOfClaimsAnswer = numberOfClaims
      )
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

      "there is no journey status in the session" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        def performAction(): Future[Result] = controller.enterDuplicateMrn(journeyBindable)(FakeRequest())

        val (session, _, _) = sessionWithClaimState(None, Some(numberOfClaims))

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

    "Render the Duplicate Entry Number page" must {
      def performAction(journeyBindable: JourneyBindable): Future[Result] =
        controller.enterDuplicateMrn(journeyBindable)(FakeRequest())

      "Show the title for the Entry Number page" in forAll(testCases) { (numberOfClaims, journey) =>
        val (session, _, _) = sessionWithClaimState(sampleEntryNumberAnswer(), Some(numberOfClaims))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val doc = Jsoup.parse(contentAsString(performAction(journey)))

        doc.select("h1").text should include(
          messageFromMessageKey(s"enter-duplicate-movement-reference-number.entry.title")
        )
      }

      "Show the title for the MRN page" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        val (session, _, _) = sessionWithClaimState(sampleMrnAnswer(), Some(numberOfClaims))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val doc = Jsoup.parse(contentAsString(performAction(journeyBindable)))
        doc.select("h1").text should include(
          messageFromMessageKey(s"enter-duplicate-movement-reference-number.mrn.title")
        )
      }

    }

    "Submit the Duplicate Entry Number page" must {

      def performAction(journeyBindable: JourneyBindable, data: (String, String)*): Future[Result] =
        controller.enterDuplicateMrnSubmit(journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "Fail if the same Entry Number is submitted" in forAll(testCases) { (numberOfClaims, journey) =>
        val entryNumber     = sample[EntryNumber]
        val answers         = sampleEntryNumberAnswer(entryNumber)
        val (session, _, _) = sessionWithClaimState(answers, Some(numberOfClaims))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = performAction(journey, keyForenterDuplicateMovementReferenceNumber -> entryNumber.value)

        val doc   = Jsoup.parse(contentAsString(result))
        val error = getGlobalErrors(doc).text()
        error shouldBe messageFromMessageKey(
          s"$keyForenterDuplicateMovementReferenceNumber.entry.invalid.enter-different-entry-number"
        )

        status(result) shouldBe BAD_REQUEST
      }

      "Fail on an Entry Number journey and an MRN is submitted" in forAll(testCases) { (numberOfClaims, journey) =>
        val (session, _, _) = sessionWithClaimState(sampleEntryNumberAnswer(), Some(numberOfClaims))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = performAction(journey, keyForenterDuplicateMovementReferenceNumber -> sample[MRN].value)

        val doc   = Jsoup.parse(contentAsString(result))
        val error = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey(
          s"$keyForenterDuplicateMovementReferenceNumber.entry.invalid.entry-number-not-mrn"
        )
        status(result) shouldBe BAD_REQUEST
      }

      "Redirect to the enter duplicate declaration details page if a different Entry Number is submitted" in forAll(
        testCases
      ) { (numberOfClaims, journey) =>
        val entryNumber     = sample[EntryNumber]
        val answers         = sampleEntryNumberAnswer(entryNumber)
        val (session, _, _) = sessionWithClaimState(answers, Some(numberOfClaims))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(journey, keyForenterDuplicateMovementReferenceNumber -> genOtherThan(entryNumber).value),
          routes.EnterDeclarationDetailsController.enterDuplicateDeclarationDetails()
        )
      }

      "Fail if the same MRN is submitted" in forAll(testCases) { (numberOfClaims, journeyBindable) =>
        val mrn             = sample[MRN]
        val answers         = sampleMrnAnswer(mrn)
        val (session, _, _) = sessionWithClaimState(answers, Some(numberOfClaims))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = performAction(journeyBindable, keyForenterDuplicateMovementReferenceNumber -> mrn.value)

        val doc   = Jsoup.parse(contentAsString(result))
        val error = getGlobalErrors(doc).text()
        error          shouldBe messageFromMessageKey(
          s"$keyForenterDuplicateMovementReferenceNumber.mrn.invalid.enter-different-mrn"
        )
        status(result) shouldBe BAD_REQUEST
      }

      "Fail on an MRN journey and an entry number is submitted" in forAll(testCases) {
        (numberOfClaims, journeyBindable) =>
          val (session, _, _) = sessionWithClaimState(sampleMrnAnswer(), Some(numberOfClaims))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          val result          =
            performAction(journeyBindable, keyForenterDuplicateMovementReferenceNumber -> sample[EntryNumber].value)

          val doc             = Jsoup.parse(contentAsString(result))
          val error           = getGlobalErrors(doc).text()
          error          shouldBe messageFromMessageKey(
            s"$keyForenterDuplicateMovementReferenceNumber.mrn.invalid.mrn-not-entry-number"
          )
          status(result) shouldBe BAD_REQUEST
      }

      "Redirect to the enter importer eori page when a different MRN Number is submitted " in forAll(
        testCases
      ) { (numberOfClaims, journeyBindable) =>
        val mrn                = sample[MRN]
        val answers            = sampleMrnAnswer(mrn)
        val (session, _, _)    = sessionWithClaimState(answers, Some(numberOfClaims))
        val displayDeclaration = sample[DisplayDeclaration]
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(journeyBindable, keyForenterDuplicateMovementReferenceNumber -> genOtherThan(mrn).value),
          routes.EnterImporterEoriNumberController.enterImporterEoriNumber()
        )
      }

      "Redirect to Check Duplicate Declaration details, when the logged in user is the importer" in {
        val mrn               = sample[MRN]
        val answers           = sampleMrnAnswer(mrn)
        val (session, foc, _) = sessionWithClaimState(answers, Some(SelectNumberOfClaimsAnswer.Individual))

        val consigneeDetails   = sample[ConsigneeDetails].copy(consigneeEORI = foc.signedInUserDetails.eori.value)
        val displayDeclaration = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(JourneyBindable.Single, keyForenterDuplicateMovementReferenceNumber -> genOtherThan(mrn).value),
          routes.CheckDuplicateDeclarationDetailsController.show(JourneyBindable.Single)
        )

      }

    }

    "MRN Form validation" must {

      "accept valid MRN" in {
        val mrn    = sample[MRN]
        val errors =
          mrnForm(mrn).bind(Map(keyForenterDuplicateMovementReferenceNumber -> genOtherThan(mrn).value)).errors
        errors shouldBe Nil
      }

      "reject 19 characters" in {
        val errors =
          mrnForm(sample[MRN]).bind(Map(keyForenterDuplicateMovementReferenceNumber -> "910ABCDEFGHIJKLMNO0")).errors
        errors.headOption.value.messages shouldBe List("invalid.number")
      }

      "reject 17 characters" in {
        val mrn    = sample[MRN]
        val errors = mrnForm(mrn).bind(Map(keyForenterDuplicateMovementReferenceNumber -> "123456789A1234567")).errors
        errors.headOption.value.messages shouldBe List("invalid.number")
      }

      "reject empty MRN field" in {
        val errors = mrnForm(sample[MRN]).bind(Map(keyForenterDuplicateMovementReferenceNumber -> " ")).errors
        errors.headOption.value.messages shouldBe List("error.required")
      }

      "reject duplicate MRN when it's the same as the main MRN" in {
        val mrn    = sample[MRN]
        val errors = mrnForm(mrn).bind(Map(keyForenterDuplicateMovementReferenceNumber -> mrn.value)).errors
        errors.headOption.value.messages shouldBe List("invalid.enter-different-mrn")
      }

      "reject Entry Number on the MRN form" in {
        val errors =
          mrnForm(sample[MRN])
            .bind(Map(keyForenterDuplicateMovementReferenceNumber -> sample[EntryNumber].value))
            .errors
        errors.headOption.value.messages shouldBe List("invalid.mrn-not-entry-number")
      }
    }

    "EntryNumber Form validation" must {

      "accept valid Entry Number (Chief Number)" in {
        val entryNumber = sample[EntryNumber]
        val errors      = entryForm(entryNumber)
          .bind(Map(keyForenterDuplicateMovementReferenceNumber -> genOtherThan(entryNumber).value))
          .errors
        errors shouldBe Nil
      }

      "reject 19 characters" in {
        val errors = entryForm(sample[EntryNumber])
          .bind(Map(keyForenterDuplicateMovementReferenceNumber -> "910ABCDEFGHIJKLMNO0"))
          .errors
        errors.headOption.value.messages shouldBe List("invalid.number")
      }

      "reject 17 characters" in {
        val errors = entryForm(sample[EntryNumber])
          .bind(Map(keyForenterDuplicateMovementReferenceNumber -> "123456789A1234567"))
          .errors
        errors.headOption.value.messages shouldBe List("invalid.number")
      }

      "reject empty Entry Number field" in {
        val errors = entryForm(sample[EntryNumber]).bind(Map(keyForenterDuplicateMovementReferenceNumber -> " ")).errors
        errors.headOption.value.messages shouldBe List("error.required")
      }

      "reject duplicate Entry Number when it's the same as the main Entry Number" in {
        val entryNumber = sample[EntryNumber]
        val errors      =
          entryForm(entryNumber).bind(Map(keyForenterDuplicateMovementReferenceNumber -> entryNumber.value)).errors
        errors.headOption.value.messages shouldBe List("invalid.enter-different-entry-number")
      }

      "reject MRN on the Entry Number form" in {
        val errors =
          entryForm(sample[EntryNumber])
            .bind(Map(keyForenterDuplicateMovementReferenceNumber -> sample[MRN].value))
            .errors
        errors.headOption.value.messages shouldBe List("invalid.entry-number-not-mrn")
      }

    }

  }
}
