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

import cats.{Functor, Id}
import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache

import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaim.{IncorrectExciseValue, PersonalEffects}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaimAnswer.CompleteBasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutiesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{DisplayDeclaration, NdrcDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, SignedInUserDetails, _}

import scala.concurrent.Future
import scala.util.Random

class SelectDutiesControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: SelectDutiesController = instanceOf[SelectDutiesController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def getMRNAnswer(): MovementReferenceNumber         = MovementReferenceNumber(Right(sample[MRN]))
  def getEntryNumberAnswer(): MovementReferenceNumber = MovementReferenceNumber(
    Left(sample[EntryNumber])
  )

  private def getSessionWithPreviousAnswer(
    maybeDutiesSelectedAnswer: Option[DutiesSelectedAnswer],
    movementReferenceNumber: MovementReferenceNumber,
    displayDeclaration: Option[DisplayDeclaration] = None,
    basisOfClaim: BasisOfClaimAnswer = CompleteBasisOfClaimAnswer(PersonalEffects)
  ): (SessionData, FillingOutClaim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(
      dutiesSelectedAnswer = maybeDutiesSelectedAnswer,
      movementReferenceNumber = Some(movementReferenceNumber),
      displayDeclaration = displayDeclaration,
      basisOfClaimAnswer = Some(basisOfClaim)
    )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey
    )
  }

  private def updateSession(sessionData: SessionData, dutiesSelectedAnswer: DutiesSelectedAnswer): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, (draftClaim: DraftC285Claim))) =>
        val newClaim      =
          draftClaim.copy(dutiesSelectedAnswer = Some(dutiesSelectedAnswer))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                         => fail()
    }

  def isA00Checked(document: Document): Boolean =
    isChecked(document, TaxCode.A00.value)

  def isA30Checked(document: Document): Boolean =
    isChecked(document, TaxCode.A30.value)

  def isA90Checked(document: Document): Boolean =
    isChecked(document, TaxCode.A90.value)

  def isB00Checked(document: Document): Boolean =
    isChecked(document, TaxCode.B00.value)

  def isChecked(document: Document, fieldValue: String): Boolean =
    document.select(s"""input[value="$fieldValue"] """).hasAttr("checked")

  def getBackLink(document: Document): String =
    document.select("a.govuk-back-link").attr("href")

  "Select Duties Controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        def performAction(): Future[Result] = controller.selectDuties()(FakeRequest())

        val session = getSessionWithPreviousAnswer(None, getEntryNumberAnswer())._1

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

    "display the page" when {

      def performAction(): Future[Result] = controller.selectDuties()(FakeRequest())

      "the user has not answered this question before" in {
        val session = getSessionWithPreviousAnswer(None, getEntryNumberAnswer())._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-duties.title"),
          doc => {
            isA00Checked(doc) shouldBe false
            isA30Checked(doc) shouldBe false
            isA90Checked(doc) shouldBe false
            isB00Checked(doc) shouldBe false
          }
        )
      }

      "the user has answered this question before with a single choice" in {
        val previousAnswer = DutiesSelectedAnswer(Duty(TaxCode.A00))
        val session        = getSessionWithPreviousAnswer(Some(previousAnswer), getEntryNumberAnswer())._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-duties.title"),
          doc => {
            isA00Checked(doc) shouldBe true
            isA30Checked(doc) shouldBe false
            isA90Checked(doc) shouldBe false
            isB00Checked(doc) shouldBe false
          }
        )
      }

      "the user has answered this question before with a multiple choices" in {
        val previousAnswer = DutiesSelectedAnswer(Duty(TaxCode.A00), Duty(TaxCode.A90), Duty(TaxCode.B00))
        val session        = getSessionWithPreviousAnswer(Some(previousAnswer), getEntryNumberAnswer())._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-duties.title"),
          doc => {
            isA00Checked(doc) shouldBe true
            isA30Checked(doc) shouldBe false
            isA90Checked(doc) shouldBe true
            isB00Checked(doc) shouldBe true
          }
        )
      }

      "the user has answered this question before with a choice, but that choice is no longer available (e.g. Northern Ireland answer change)" in {
        val previousTaxCodes = Random.shuffle(TaxCode.listOfUKTaxCodes).take(3)
        val previousAnswer   = DutiesSelectedAnswer(previousTaxCodes.map(Duty(_))).getOrElse(fail)
        val newTaxCodes      = Random.shuffle(TaxCode.listOfUKExciseCodes).take(3)
        val ndrcs            = newTaxCodes.map(code => sample[NdrcDetails].copy(taxType = code.value))
        val acc14            = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
        )
        val basisOfClaim     = CompleteBasisOfClaimAnswer(IncorrectExciseValue)

        val session =
          getSessionWithPreviousAnswer(Some(previousAnswer), getEntryNumberAnswer(), Some(acc14), basisOfClaim)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-duties.title"),
          doc => {
            isA00Checked(doc) shouldBe false
            isA30Checked(doc) shouldBe false
            isA90Checked(doc) shouldBe false
            isB00Checked(doc) shouldBe false
          }
        )
      }

    }

    "handle submit requests" when {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.selectDutiesSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "user chooses a valid option" in {
        val answers        = DutiesSelectedAnswer(Duty(TaxCode.A00), Duty(TaxCode.A20))
        val session        = getSessionWithPreviousAnswer(None, getEntryNumberAnswer())._1
        val updatedSession = updateSession(session, answers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(
            Seq("select-duties[]" -> "A00", "select-duties[]" -> "A20")
          ),
          routes.EnterClaimController.startClaim()
        )
      }

    }

    "show an error summary" when {
      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.selectDutiesSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "an invalid option value is submitted" in {
        val session = getSessionWithPreviousAnswer(None, getEntryNumberAnswer())._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(
            Seq("select-duties" -> "XXX")
          ),
          messageFromMessageKey("select-duties.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(s"select-duties.error.required"),
          BAD_REQUEST
        )
      }

    }

    "Available Duties" should {
      "Return all UK and EU duties for an Entry Number" in {
        val session         = getSessionWithPreviousAnswer(None, getEntryNumberAnswer())._2
        val dutiesAvailable = SelectDutiesController.getAvailableDuties(session)
        dutiesAvailable shouldBe DutiesSelectedAnswer(TaxCode.ukAndEuTaxCodes.map(Duty(_))).toRight(fail())
      }

      "Return Acc14 duties for an MRN" in {
        val taxCodes        = Random.shuffle(TaxCode.allTaxCodes).take(20)
        val ndrcs           = taxCodes.map(code => sample[NdrcDetails].copy(taxType = code.value))
        val acc14           = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
        )
        val session         = getSessionWithPreviousAnswer(None, getMRNAnswer(), Some(acc14))._2
        val dutiesAvailable = SelectDutiesController.getAvailableDuties(session)
        dutiesAvailable.map(_.toList) shouldBe Right(taxCodes.map(Duty(_)))
      }

      "Return Acc14 excise codes for an MRN when the Incorrect Excise code was selected previously" in {
        val basisOfClaim    = CompleteBasisOfClaimAnswer(IncorrectExciseValue)
        val taxCodes        = Random.shuffle(TaxCode.listOfUKExciseCodes).take(3)
        val ndrcs           = taxCodes.map(code => sample[NdrcDetails].copy(taxType = code.value))
        val acc14           = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
        )
        val session         = getSessionWithPreviousAnswer(None, getMRNAnswer(), Some(acc14), basisOfClaim)._2
        val dutiesAvailable = SelectDutiesController.getAvailableDuties(session)
        dutiesAvailable.map(_.toList) shouldBe Right(taxCodes.map(Duty(_)))
      }

    }

  }

}
