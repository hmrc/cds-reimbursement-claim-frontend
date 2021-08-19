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

import cats.syntax.all._
import org.scalatest.OptionValues
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.OK
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarationDetailsController.{EntryDeclarationDetails, entryDeclarationDetailsForm}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer.CompleteDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, GGCredId, MRN}
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import play.api.test.FakeRequest
import play.api.test.Helpers.{BAD_REQUEST, defaultAwaitTimeout, status}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache

import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer.IncompleteDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DuplicateDeclarationDetailsAnswer.{CompleteDuplicateDeclarationDetailsAnswer, IncompleteDuplicateDeclarationDetailAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EnterDeclarationDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, SignedInUserDetails, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class EnterDeclarationDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with OptionValues
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val featureSwitch = instanceOf[FeatureSwitchService]

  featureSwitch.EntryNumber.enable()

  lazy val controller: EnterDeclarationDetailsController = instanceOf[EnterDeclarationDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithDeclaration(
    maybeDeclarationDetailsAnswer: Option[DeclarationDetailsAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(declarationDetailsAnswer = maybeDeclarationDetailsAnswer)
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

  private def sessionWithDuplicateDeclaration(
    maybeDeclarationDetailsAnswer: Option[DuplicateDeclarationDetailsAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(duplicateDeclarationDetailsAnswer = maybeDeclarationDetailsAnswer)
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

  def performAction(declaration: EntryDeclarationDetails)(action: Action[AnyContent]): Future[Result] =
    performAction(entryDeclarationDetailsForm.fillAndValidate(declaration).data.toSeq)(action)

  def performAction(data: Seq[(String, String)] = Seq.empty)(action: Action[AnyContent]): Future[Result] =
    action()(FakeRequest().withFormUrlEncodedBody(data: _*))

  "Enter Declaration Details controller" must {

    "redirect to the start of the journey" when {
      "there is no journey status in the session" in {

        val answers = IncompleteDeclarationDetailsAnswer.empty

        val (session, _, _) = sessionWithDeclaration(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction()(controller.changeDeclarationDetails()),
          baseRoutes.StartController.start()
        )
      }
    }

    "show an error summary" when {

      "the user does not select any options" in {

        val answers = CompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails])

        val draftC285Claim                = sessionWithDeclaration(Some(answers))._3
          .copy(movementReferenceNumber = sampleEntryNumberAnswer())
        val (session, fillingOutClaim, _) = sessionWithDeclaration(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction()(controller.enterDeclarationDetailsSubmit()),
          messageFromMessageKey("enter-declaration-details.title"),
          doc => {
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(2) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.place-of-import.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(3) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.importer-name.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(4) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.importer-email-address.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(5) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.importer-phone-number.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(6) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.declarant-name.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(7) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.declarant-email-address.error.required"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(8) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.declarant-phone-number.error.required"
            )
          },
          BAD_REQUEST
        )
      }

      "an invalid option value too long is submitted" in {

        val answers = CompleteDeclarationDetailsAnswer(
          sample[EntryDeclarationDetails]
        )

        val draftC285Claim                = sessionWithDeclaration(Some(answers))._3
          .copy(movementReferenceNumber = sampleEntryNumberAnswer())
        val (session, fillingOutClaim, _) = sessionWithDeclaration(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(
            Seq(
              "enter-declaration-details.place-of-import"         -> List.fill(71)("a").mkString(""),
              "enter-declaration-details.importer-name"           -> List.fill(71)("a").mkString(""),
              "enter-declaration-details.importer-email-address"  -> List.fill(250)("a").mkString(""),
              "enter-declaration-details.importer-phone-number"   -> List.fill(31)("1").mkString(""),
              "enter-declaration-details.declarant-name"          -> List.fill(71)("a").mkString(""),
              "enter-declaration-details.declarant-email-address" -> List.fill(250)("a").mkString(""),
              "enter-declaration-details.declarant-phone-number"  -> List.fill(31)("1").mkString("")
            )
          )(controller.enterDeclarationDetailsSubmit()),
          messageFromMessageKey("enter-declaration-details.title"),
          doc => {
            doc
              .select(".govuk-error-summary__list > li:nth-child(2) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.place-of-import.error.maxLength"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(3) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.importer-name.error.maxLength"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(4) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.importer-email-address.error.maxLength"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(5) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.importer-phone-number.error.maxLength"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(6) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.declarant-name.error.maxLength"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(7) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.declarant-email-address.error.maxLength"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(8) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.declarant-phone-number.error.maxLength"
            )
          },
          BAD_REQUEST
        )

      }

      "a phone number with chars is submitted" in {

        val answers = CompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails])

        val draftC285Claim                = sessionWithDeclaration(Some(answers))._3
          .copy(movementReferenceNumber = sampleEntryNumberAnswer())
        val (session, fillingOutClaim, _) = sessionWithDeclaration(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(
            Seq(
              "enter-declaration-details.importer-email-address"  -> "myemail",
              "enter-declaration-details.importer-phone-number"   -> "123456789a",
              "enter-declaration-details.declarant-email-address" -> "myotheremail",
              "enter-declaration-details.declarant-phone-number"  -> "123456789a"
            )
          )(controller.enterDeclarationDetailsSubmit()),
          messageFromMessageKey("enter-declaration-details.title"),
          doc => {
            doc
              .select(".govuk-error-summary__list > li:nth-child(4) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.importer-email-address.invalid"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(5) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.importer-phone-number.invalid"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(7) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.declarant-email-address.invalid"
            )
            doc
              .select(".govuk-error-summary__list > li:nth-child(8) > a")
              .text() shouldBe messageFromMessageKey(
              s"enter-declaration-details.declarant-phone-number.invalid"
            )
          },
          BAD_REQUEST
        )

      }
    }

    "redirect to enter reference page" when {

      "MRN is provided instead Entry number to fill declaration" in new TableDrivenPropertyChecks {
        val testCases = Table(
          ("Controller action", "Maybe Answers"),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.enterDeclarationDetails()),
            IncompleteDeclarationDetailsAnswer(None)
          ),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.enterDeclarationDetails()),
            IncompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails].some)
          ),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.enterDeclarationDetails()),
            CompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails])
          ),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.enterDeclarationDetailsSubmit()),
            CompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails])
          ),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.changeDeclarationDetails()),
            IncompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails].some)
          ),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.changeDeclarationDetails()),
            IncompleteDeclarationDetailsAnswer(None)
          ),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.changeDeclarationDetails()),
            CompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails])
          ),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.changeDeclarationDetailsSubmit()),
            IncompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails].some)
          )
        )

        forAll(testCases) { (action, answers) =>
          val mrn = sample[MRN]

          val (session, fillingOutClaim, draftC285Claim) = sessionWithDeclaration(Some(answers))

          val updatedJourney = fillingOutClaim.copy(draftClaim =
            draftC285Claim.copy(movementReferenceNumber = Some(MovementReferenceNumber(mrn.asRight[EntryNumber])))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          }

          checkIsRedirect(
            action(Seq()),
            routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single)
          )
        }
      }

      "MRN is provided instead of Entry number to fill duplicate declaration" in new TableDrivenPropertyChecks {
        val testCases = Table(
          ("Controller action", "Maybe Answers"),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.enterDuplicateDeclarationDetails()),
            IncompleteDuplicateDeclarationDetailAnswer(sample[EntryDeclarationDetails].some)
          ),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.enterDuplicateDeclarationDetails()),
            IncompleteDuplicateDeclarationDetailAnswer(None)
          ),
          (
            (data: Seq[(String, String)]) => performAction(data)(controller.enterDuplicateDeclarationDetailsSubmit()),
            CompleteDuplicateDeclarationDetailsAnswer(sample[EntryDeclarationDetails].some)
          )
        )

        forAll(testCases) { (action, answer) =>
          val mrn = sample[MRN]

          val (session, fillingOutClaim, draftC285Claim) = sessionWithDuplicateDeclaration(Some(answer))

          val updatedJourney = fillingOutClaim.copy(draftClaim =
            draftC285Claim.copy(movementReferenceNumber = Some(MovementReferenceNumber(mrn.asRight[EntryNumber])))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          }

          checkIsRedirect(
            action(Seq()),
            routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single)
          )
        }
      }
    }

    "follow up with the next enter declaration page" when {
      "current form is submitted" in new TableDrivenPropertyChecks {
        val testCases = Table(
          ("An action", "Page to redirect"),
          (
            controller.enterDeclarationDetailsSubmit(),
            routes.SelectWhoIsMakingTheClaimController.selectDeclarantType(JourneyBindable.Single)
          ),
          (
            controller.changeDeclarationDetailsSubmit(),
            routes.CheckYourAnswersAndSubmitController.checkAllAnswers(JourneyBindable.Single)
          )
        )

        forAll(testCases) { (action, redirectPage) =>
          val declarationDetails = sample[EntryDeclarationDetails]
          val answers            = IncompleteDeclarationDetailsAnswer(declarationDetails.some)

          val (session, fillingOutClaim, draftC285Claim) = sessionWithDeclaration(Some(answers))

          val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performAction(declarationDetails)(action),
            redirectPage
          )
        }
      }
    }

    "redirect to enter commodities details" when {
      "duplicate declaration is submitted" in {
        val declarationDetails = sample[EntryDeclarationDetails]
        val answers            = IncompleteDuplicateDeclarationDetailAnswer(declarationDetails.some)

        val (session, fillingOutClaim, draftC285Claim) = sessionWithDuplicateDeclaration(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(declarationDetails)(controller.enterDuplicateDeclarationDetailsSubmit()),
          routes.EnterCommoditiesDetailsController.enterCommoditiesDetails(JourneyBindable.Single)
        )
      }
    }

    "show enter declaration details form" when {
      "the user is adding new or changing existing data" in new TableDrivenPropertyChecks {
        val testCases = Table(
          ("Action", "Answers"),
          (
            controller.enterDeclarationDetails(),
            IncompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails].some)
          ),
          (controller.enterDeclarationDetails(), IncompleteDeclarationDetailsAnswer(None)),
          (controller.enterDeclarationDetails(), CompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails])),
          (
            controller.changeDeclarationDetails(),
            IncompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails].some)
          ),
          (controller.changeDeclarationDetails(), IncompleteDeclarationDetailsAnswer(None)),
          (controller.changeDeclarationDetails(), CompleteDeclarationDetailsAnswer(sample[EntryDeclarationDetails]))
        )

        forAll(testCases) { (action, answer) =>
          val entryNumber = sample[EntryNumber]

          val (session, fillingOutClaim, draftC285Claim) = sessionWithDeclaration(Some(answer))

          val updatedJourney = fillingOutClaim.copy(draftClaim =
            draftC285Claim.copy(movementReferenceNumber = Some(MovementReferenceNumber(entryNumber.asLeft[MRN])))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          }

          checkPageIsDisplayed(
            performAction(Seq())(action),
            messageFromMessageKey("enter-declaration-details.title")
          )
        }
      }

      "the user is adding duplicate declaration" in new TableDrivenPropertyChecks {
        val testCases = Table(
          ("Action", "Answers"),
          (
            controller.enterDuplicateDeclarationDetails(),
            IncompleteDuplicateDeclarationDetailAnswer(sample[EntryDeclarationDetails].some)
          ),
          (controller.enterDuplicateDeclarationDetails(), IncompleteDuplicateDeclarationDetailAnswer(None)),
          (
            controller.enterDuplicateDeclarationDetails(),
            CompleteDuplicateDeclarationDetailsAnswer(sample[EntryDeclarationDetails].some)
          ),
          (controller.enterDuplicateDeclarationDetails(), CompleteDuplicateDeclarationDetailsAnswer(None))
        )

        forAll(testCases) { (action, answer) =>
          val entryNumber = sample[EntryNumber]

          val (session, fillingOutClaim, draftC285Claim) = sessionWithDuplicateDeclaration(Some(answer))

          val updatedJourney = fillingOutClaim.copy(draftClaim =
            draftC285Claim.copy(movementReferenceNumber = Some(MovementReferenceNumber(entryNumber.asLeft[MRN])))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          }

          status(performAction(Seq())(action)) shouldBe OK
        }
      }
    }

    "reject to submit invalid form" when {
      "the users is entering or editing declaration" in new TableDrivenPropertyChecks {
        val actions = Table(
          "An action",
          controller.enterDeclarationDetailsSubmit(),
          controller.changeDeclarationDetailsSubmit()
        )

        forAll(actions) { action =>
          val entryNumber        = sample[EntryNumber]
          val declarationDetails = sample[EntryDeclarationDetails].copy(
            declarantPhoneNumber = PhoneNumber("a")
          )

          val (session, fillingOutClaim, draftC285Claim) = sessionWithDeclaration(
            Some(IncompleteDeclarationDetailsAnswer(declarationDetails.some))
          )

          val updatedJourney = fillingOutClaim.copy(draftClaim =
            draftC285Claim.copy(movementReferenceNumber = Some(MovementReferenceNumber(entryNumber.asLeft[MRN])))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
          }

          status(performAction(declarationDetails)(action)) shouldBe BAD_REQUEST
        }
      }

      "the user is entering duplicate declaration" in {
        val entryNumber        = sample[EntryNumber]
        val declarationDetails = sample[EntryDeclarationDetails].copy(
          declarantPhoneNumber = PhoneNumber("a")
        )

        val (session, fillingOutClaim, draftC285Claim) = sessionWithDuplicateDeclaration(
          Some(IncompleteDuplicateDeclarationDetailAnswer(declarationDetails.some))
        )

        val updatedJourney = fillingOutClaim.copy(draftClaim =
          draftC285Claim.copy(movementReferenceNumber = Some(MovementReferenceNumber(entryNumber.asLeft[MRN])))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        status(
          performAction(declarationDetails)(controller.enterDuplicateDeclarationDetailsSubmit())
        ) shouldBe BAD_REQUEST
      }
    }
  }

  "Form Validation" must {
    val form              = EnterDeclarationDetailsController.entryDeclarationDetailsForm
    val dateOfImportDay   = "enter-declaration-details.day"
    val dateOfImportMonth = "enter-declaration-details.month"
    val dateOfImportYear  = "enter-declaration-details.year"

    val goodData = Map(
      dateOfImportDay   -> "20",
      dateOfImportMonth -> "3",
      dateOfImportYear  -> "1987"
    )

    val emptyData = Map(
      dateOfImportDay   -> " ",
      dateOfImportMonth -> " ",
      dateOfImportYear  -> " "
    )

    "Day and month" should {
      "Reject empty day and month" in {
        val errors = form.bind(emptyData.updated(dateOfImportYear, "1987")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("dayAndMonth.error.required")
      }
    }

    "Day and year" should {
      "Reject empty day and year" in {
        val errors = form.bind(emptyData.updated(dateOfImportMonth, "3")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("dayAndYear.error.required")
      }
    }

    "Month and year" should {
      "Reject empty month and year" in {
        val errors = form.bind(emptyData.updated(dateOfImportDay, "20")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("monthAndYear.error.required")
      }
    }

    "Day of Import" should {
      "Reject empty day" in {
        val errors = form.bind(goodData.updated(dateOfImportDay, " ")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("day.error.required")
      }

      "Reject days too big" in {
        val errors = form.bind(goodData.updated(dateOfImportDay, "32")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject days too small" in {
        val errors = form.bind(goodData.updated(dateOfImportDay, "0")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject valid days in 3 digits" in {
        val errors = form.bind(goodData.updated(dateOfImportDay, "015")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject days with chars" in {
        val errors = form.bind(goodData.updated(dateOfImportDay, "Ab")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }
    }

    "Month of Import" should {

      "Reject empty month" in {
        val errors = form.bind(goodData.updated(dateOfImportMonth, " ")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("month.error.required")
      }

      "Reject months too big" in {
        val errors = form.bind(goodData.updated(dateOfImportMonth, "13")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject months too small" in {
        val errors = form.bind(goodData.updated(dateOfImportMonth, "0")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject valid months in 3 digits" in {
        val errors = form.bind(goodData.updated(dateOfImportMonth, "012")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject months with chars" in {
        val errors = form.bind(goodData.updated(dateOfImportMonth, "Ja")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

    }

    "Year of Import" should {

      "Reject empty year" in {
        val errors = form.bind(goodData.updated(dateOfImportYear, " ")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("year.error.required")
      }

      "Reject years too far" in {
        val errors = form.bind(goodData.updated(dateOfImportYear, "2120")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.tooFarInFuture")
      }

      "Reject years too early" in {
        val errors = form.bind(goodData.updated(dateOfImportYear, "1899")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.before1900")
      }

      "Reject 3 digits" in {
        val errors = form.bind(goodData.updated(dateOfImportYear, "202")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Reject years with chars" in {
        val errors = form.bind(goodData.updated(dateOfImportYear, "202a")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

    }
  }
}
