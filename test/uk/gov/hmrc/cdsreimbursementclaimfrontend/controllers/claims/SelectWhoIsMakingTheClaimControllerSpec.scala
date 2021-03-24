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
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumberAnswer.CompleteMovementReferenceNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DeclarantTypeAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.EntryNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarantTypeAnswer, DraftClaim, SessionData}

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

  def sessionWithFillingOutClaim(
    answers: Option[DeclarantTypeAnswer]
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftClaim = sample[DraftC285Claim].copy(
      declarantTypeAnswer = answers,
      movementReferenceNumberAnswer = Some(CompleteMovementReferenceNumberAnswer(Left(EntryNumber(""))))
    )
    val journey    = sample[FillingOutClaim].copy(draftClaim = draftClaim)

    val session = SessionData.empty.copy(journeyStatus = Some(journey))
    (session, journey, draftClaim)
  }

  "Select who is making the claim controller" when {

    "handling request to show the page" must {

      def performAction(): Future[Result] = controller.selectDeclarantType()(FakeRequest())

      "show the page" when {

        def test(
          sessionData: SessionData,
          expectedTitleKey: String,
          expectedBackLink: Call,
          expectedActionLink: Call
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action")                shouldBe expectedActionLink.url
            }
          )
        }

        "the user is filling in a claim" in {

          val declarantTypeAnswer = sample[DeclarantTypeAnswer]
          val session             = sessionWithFillingOutClaim(Some(declarantTypeAnswer))._1

          test(
            session,
            "select-who-is-making-the-claim.title",
            routes.EnterDeclarationDetailsController.enterDeclarationDetails(),
            routes.SelectWhoIsMakingTheClaimController.selectDeclarantTypeSubmit()
          )

        }

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
