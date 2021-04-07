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

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarationDetailsController.EntryDeclarationDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer.CompleteDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.CompleteMovementReferenceNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.EntryNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer.IncompleteDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, SignedInUserDetails, _}

import java.time.LocalDate
import scala.concurrent.Future

class EnterDeclarationDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: EnterDeclarationDetailsController = instanceOf[EnterDeclarationDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
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

  "Enter Declaration Details controller" must {

    "redirect to the start of the journey" when {
      "there is no journey status in the session" in {

        def performAction(): Future[Result] = controller.changeDeclarationDetails()(FakeRequest())

        val answers = IncompleteDeclarationDetailsAnswer.empty

        val (session, _, _) = sessionWithClaimState(Some(answers))

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

    "show an error summary" when {

      "the user does not select any options" in {

        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterDeclarationDetailsSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val date           = DateOfImport(LocalDate.of(2020, 10, 31))
        val place          = "Newtown"
        val importerName   = "The Importer"
        val importerEmail  = Email("importer@mac.com")
        val importerPhone  = PhoneNumber("07954673645")
        val declarantName  = "Gertrude Blunderbuss"
        val declarantEmail = Email("GF@mac.com")
        val declarantPhone = PhoneNumber("08689453645")

        val answers = CompleteDeclarationDetailsAnswer(
          EntryDeclarationDetails(
            date,
            place,
            importerName,
            importerEmail,
            importerPhone,
            declarantName,
            declarantEmail,
            declarantPhone
          )
        )

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(movementReferenceNumberAnswer =
            Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(
            Seq.empty
          ),
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
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterDeclarationDetailsSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val date           = DateOfImport(LocalDate.of(2020, 10, 31))
        val place          = "Newtown"
        val importerName   = "The Importer"
        val importerEmail  = Email("importer@mac.com")
        val importerPhone  = PhoneNumber("07954673645")
        val declarantName  = "Gertrude Blunderbuss"
        val declarantEmail = Email("GF@mac.com")
        val declarantPhone = PhoneNumber("08689453645")

        val answers = CompleteDeclarationDetailsAnswer(
          EntryDeclarationDetails(
            date,
            place,
            importerName,
            importerEmail,
            importerPhone,
            declarantName,
            declarantEmail,
            declarantPhone
          )
        )

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(movementReferenceNumberAnswer =
            Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

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
          ),
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
        def performAction(data: Seq[(String, String)]): Future[Result] =
          controller.enterDeclarationDetailsSubmit()(
            FakeRequest().withFormUrlEncodedBody(data: _*)
          )

        val date           = DateOfImport(LocalDate.of(2020, 10, 31))
        val place          = "Newtown"
        val importerName   = "The Importer"
        val importerEmail  = Email("importer@mac.com")
        val importerPhone  = PhoneNumber("07954673645")
        val declarantName  = "Gertrude Blunderbuss"
        val declarantEmail = Email("GF@mac.com")
        val declarantPhone = PhoneNumber("08689453645")

        val answers = CompleteDeclarationDetailsAnswer(
          EntryDeclarationDetails(
            date,
            place,
            importerName,
            importerEmail,
            importerPhone,
            declarantName,
            declarantEmail,
            declarantPhone
          )
        )

        val draftC285Claim                = sessionWithClaimState(Some(answers))._3
          .copy(movementReferenceNumberAnswer =
            Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber("entry-num"))))
          )
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(answers))

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
          ),
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

    "Day of Import" should {
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
