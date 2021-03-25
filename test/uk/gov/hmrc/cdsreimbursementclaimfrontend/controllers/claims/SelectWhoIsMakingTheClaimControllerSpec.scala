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
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.{CompleteDeclarantTypeAnswer, IncompleteDeclarantTypeAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.CompleteMovementReferenceNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DeclarantTypeAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, GGCredId}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference

import scala.concurrent.Future

class SelectWhoIsMakingTheClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: SelectWhoIsMakingTheClaimController = instanceOf[SelectWhoIsMakingTheClaimController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    declarantTypeAnswer: Option[DeclarantTypeAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(declarantTypeAnswer = declarantTypeAnswer)
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails =
      SignedInUserDetails(Some(email), eori, Email("email@email.com"), ContactName("Fred Bread"))
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftC285Claim
    )
  }

  def testFormError(
    uploadReference: UploadReference,
    data: (String, String)*
  )(
    expectedErrorMessageKey: String,
    errorArgs: Seq[String] = Nil
  )(pageTitleKey: String, titleArgs: String*)(
    performAction: (UploadReference, Seq[(String, String)]) => Future[Result],
    currentSession: SessionData = sessionWithClaimState(
      Some(sample[CompleteDeclarantTypeAnswer])
    )._1
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }
    checkPageIsDisplayed(
      performAction(uploadReference, data),
      messageFromMessageKey(pageTitleKey, titleArgs: _*),
      { doc =>
        doc
          .select("#error-summary-display > ul > li > a")
          .text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs: _*
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }

  "Select who is making the claim controller" when {

    def performAction(): Future[Result] = controller.selectDeclarantType()(FakeRequest())

    "show who is making the declaration page" when {

      "filling out a claim" in {

        val answers = IncompleteDeclarantTypeAnswer.empty

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
          performAction(),
          messageFromMessageKey("select-who-is-making-the-claim.title"),
          doc =>
            doc
              .select("body > div.govuk-width-container > div.cds-user-banner.cds-no-border > div:nth-child(1) > a")
              .attr("href") shouldBe
              routes.EnterDeclarationDetailsController.enterDeclarationDetails().url
        )
      }
    }

  }

  "Form Validation" must {
    val form     = SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm
    val claimKey = "select-who-is-making-the-claim"

    "accept Importer" in {
      val errors = form.bind(Map(claimKey -> "0")).errors
      errors shouldBe Nil
    }

    "accept Associated with Importer Company" in {
      val errors = form.bind(Map(claimKey -> "1")).errors
      errors shouldBe Nil
    }

    "accept Associated Representative Company" in {
      val errors = form.bind(Map(claimKey -> "2")).errors
      errors shouldBe Nil
    }

    "reject invalid numbers" in {
      val errors = form.bind(Map(claimKey -> "3")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid")
    }

    "reject invalid chars" in {
      val errors = form.bind(Map(claimKey -> "a")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("error.number")
    }

  }

}
