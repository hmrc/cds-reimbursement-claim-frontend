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
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.data.FormError
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.{CompleteMovementReferenceNumberAnswer, IncompleteMovementReferenceNumberAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.CustomsDataStoreService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterMovementReferenceNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  val mockCustomsDataStoreService = mock[CustomsDataStoreService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionStore),
      bind[CustomsDataStoreService].toInstance(mockCustomsDataStoreService)
    )

  def mockGetEmail(response: Either[Error, Option[VerifiedEmail]]) =
    (mockCustomsDataStoreService
      .getEmailByEori(_: Eori)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))
      .once()

  lazy val controller: EnterMovementReferenceNumberController = instanceOf[EnterMovementReferenceNumberController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeMovementReferenceNumberAnswer: Option[MovementReferenceNumberAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(movementReferenceNumberAnswer = maybeMovementReferenceNumberAnswer)
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

  "Movement Reference Number Controller" when {

    val verifiedEmail = "jex.belaran@xmail.com"

    "Enter MRN page" must {

      def performAction(): Future[Result] = controller.enterMrn()(FakeRequest())

      "show the title" in {
        val answers         = IncompleteMovementReferenceNumberAnswer.empty
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetEmail(Right(Some(VerifiedEmail(verifiedEmail, ""))))
          mockStoreSession(Right(()))
        }
        val doc             = Jsoup.parse(contentAsString(performAction()))

        doc.select("h1").text                                    should include(messageFromMessageKey("enter-movement-reference-number.title"))
        doc.select("#enter-movement-reference-number").`val`() shouldBe ""
      }

      "Show error page when customs-data-store request fails " in {
        val answers         = IncompleteMovementReferenceNumberAnswer.empty
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetEmail(Left(Error(new Exception("Boom"))))
        }
        checkIsTechnicalErrorPage(performAction())
      }

      "Check for redirect when customs-data-store does not return an email (no email associated for the given EORI)" in {
        val answers         = IncompleteMovementReferenceNumberAnswer.empty
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetEmail(Right(None))
        }
        checkIsRedirect(performAction(), "http://localhost:9898/manage-email-cds/service/cds-reimbursement-claim")
      }

      "Check for redirect when customs-data-store does return an email" in {
        val answers         = IncompleteMovementReferenceNumberAnswer.empty
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetEmail(Right(Some(VerifiedEmail("someone@gmail.com", ""))))
          mockStoreSession(Right(()))
        }
        status(performAction()) shouldBe 200
      }

      "Check for redirect when customs-data-store does return an email, but storage fails" in {
        val answers         = IncompleteMovementReferenceNumberAnswer.empty
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetEmail(Right(Some(VerifiedEmail("someone@gmail.com", ""))))
          mockStoreSession(Left(Error(new Exception("Wham"))))
        }
        checkIsTechnicalErrorPage(performAction())
      }

    }

    "Change MRN page" must {
      def performAction(): Future[Result] = controller.changeMrn()(FakeRequest())

      "show the title and the MRN number" in {
        val mrn             = MRN("10ABCDEFGHIJKLMNO0")
        val answers         = CompleteMovementReferenceNumberAnswer(Right(mrn))
        val (session, _, _) = sessionWithClaimState(Some(answers))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetEmail(Right(Some(VerifiedEmail(verifiedEmail, ""))))
          mockStoreSession(Right(()))
        }
        val doc             = Jsoup.parse(contentAsString(performAction()))

        doc.select("h1").text                                    should include(messageFromMessageKey("enter-movement-reference-number.title"))
        doc.select("#enter-movement-reference-number").`val`() shouldBe mrn.value
      }
    }

    "Form validation" must {
      val form   = EnterMovementReferenceNumberController.movementReferenceNumberForm
      val mrnKey = "enter-movement-reference-number"

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

      "reject 17 characters" in {
        val errors = form.bind(Map(mrnKey -> "123456789A1234567")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.number")
      }
    }
  }

}
