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
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{BAD_REQUEST, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.MovementReferenceNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutiesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{DisplayDeclaration, NdrcDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimsAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{moneyGen, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, SignedInUserDetails, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import java.util.UUID
import scala.concurrent.Future
import scala.util.Random

class EnterClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckDrivenPropertyChecks {
  lazy val controller: EnterClaimController            = instanceOf[EnterClaimController]
  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.EntryNumber.enable()

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def getSessionWithPreviousAnswer(
    maybeClaimsAnswer: Option[ClaimsAnswer],
    maybeDutiesSelectedAnswer: Option[DutiesSelectedAnswer] = None,
    ndrcDetails: Option[List[NdrcDetails]] = None,
    movementReferenceNumber: MovementReferenceNumber = getMRNAnswer()
  ): (SessionData, FillingOutClaim) = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val acc14               = Functor[Id].map(sample[DisplayDeclaration])(dd =>
      dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = ndrcDetails))
    )

    val draftC285Claim = DraftC285Claim.newDraftC285Claim.copy(
      movementReferenceNumber = Some(movementReferenceNumber),
      claimsAnswer = maybeClaimsAnswer,
      dutiesSelectedAnswer = maybeDutiesSelectedAnswer,
      displayDeclaration = Some(acc14)
    )

    val journey = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey
    )
  }

  private def updateSession(sessionData: SessionData, claimsAnswer: ClaimsAnswer): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, (draftClaim: DraftC285Claim))) =>
        val newClaim      = draftClaim.copy(claimsAnswer = Some(claimsAnswer))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                         => fail()
    }

  def getMRNAnswer(): MovementReferenceNumber         = MovementReferenceNumber(Right(sample[MRN]))
  def getEntryNumberAnswer(): MovementReferenceNumber = MovementReferenceNumber(
    Left(sample[EntryNumber])
  )

  private def compareUrlsWithouthId(url1: String, url2: String): Unit = {
    def removeId(in: String) = in.substring(0, in.lastIndexOf('/'))
    removeId(url1) shouldBe removeId(url2)
  }

  "startClaim page" must {

    def performAction(): Future[Result] = controller.startClaim()(FakeRequest())

    "redirect to the start of the journey if the session is empty" in {
      val session = getSessionWithPreviousAnswer(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(),
        baseRoutes.StartController.start()
      )
    }

    "Redirect to select duties if they were not selected previously" in {
      val session = getSessionWithPreviousAnswer(None, None)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkIsRedirect(
        performAction(),
        routes.SelectDutiesController.selectDuties()
      )
    }

    "redirect to the checkClaim page if we have finished claims for all duties" in {
      val selectedTaxCodes     = Random.shuffle(TaxCode.allTaxCodes).take(10)
      val dutiesSelectedAnswer = DutiesSelectedAnswer(selectedTaxCodes.map(Duty(_)))
      val claim                = selectedTaxCodes.map(taxCode =>
        sample[Claim]
          .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = true, taxCode = taxCode.value)
      )
      val answers              = ClaimsAnswer(claim).getOrElse(fail())
      val session              = getSessionWithPreviousAnswer(Some(answers), dutiesSelectedAnswer)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkIsRedirect(
        performAction(),
        routes.EnterClaimController.checkClaim()
      )

    }

    "Redirect to the enterClaim page if we have no claim for a duty" in {
      val taxCode              = TaxCode.A20
      val dutiesSelectedAnswer = DutiesSelectedAnswer(Duty(taxCode))
      val session              = getSessionWithPreviousAnswer(None, Some(dutiesSelectedAnswer))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockStoreSession(Right(()))
      }

      val result = performAction()

      status(result) shouldBe SEE_OTHER
      compareUrlsWithouthId(
        redirectLocation(result).getOrElse(fail()),
        routes.EnterClaimController.enterClaim(UUID.randomUUID()).url
      )
    }

    "Redirect to the enterClaim page if we have an unfinished claim for a duty" in {
      val taxCode              = TaxCode.A20
      val dutiesSelectedAnswer = DutiesSelectedAnswer(Duty(taxCode))
      val claim                = sample[Claim]
        .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = taxCode.value)

      val session = getSessionWithPreviousAnswer(Some(ClaimsAnswer(claim)), Some(dutiesSelectedAnswer))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val result = performAction()

      status(result) shouldBe SEE_OTHER
      compareUrlsWithouthId(
        redirectLocation(result).getOrElse(fail()),
        routes.EnterClaimController.enterClaim(UUID.randomUUID()).url
      )
    }
  }

  "enterClaim page" must {

    def performAction(id: UUID): Future[Result] = controller.enterClaim(id)(FakeRequest())

    "redirect to the start of the journey if the session is empty" in {
      val session = getSessionWithPreviousAnswer(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(UUID.randomUUID()),
        baseRoutes.StartController.start()
      )
    }

    //TODO finish error page
    "render an error when the claim id doesn't exist anymore" in {
      val session = getSessionWithPreviousAnswer(None)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val result = performAction(UUID.randomUUID())
      status(result)          shouldBe OK
      contentAsString(result) shouldBe "This claim no longer exists"
    }

    "render when the user has not answered this question before" in {
      val taxCode = TaxCode.B05
      val claim   = sample[Claim] //An answer is created by the startClaim method, with isFilled = false
        .copy(claimAmount = BigDecimal(0), paidAmount = BigDecimal(5), isFilled = false, taxCode = taxCode.value)
      val answers = ClaimsAnswer(claim)

      val session = getSessionWithPreviousAnswer(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(claim.id),
        messageFromMessageKey("enter-claim.title", taxCode.value, "Value Added Tax")
      )
    }

    "render the previous answer when the user has answered this question before" in {
      val taxCode = TaxCode.B05
      val claim   = sample[Claim]
        .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = true, taxCode = taxCode.value)
      val answers = ClaimsAnswer(claim)

      val session = getSessionWithPreviousAnswer(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(claim.id),
        messageFromMessageKey("enter-claim.title", taxCode.value, "Value Added Tax"),
        doc => doc.getElementById("enter-claim").`val`() shouldBe "10.00"
      )
    }

  }

  "enterClaimSubmit page" must {

    def performAction(id: UUID, data: Seq[(String, String)]): Future[Result] =
      controller.enterClaimSubmit(id)(FakeRequest().withFormUrlEncodedBody(data: _*))

    "redirect to the start of the journey if the session is empty" in {
      val session = getSessionWithPreviousAnswer(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(UUID.randomUUID(), Seq.empty),
        baseRoutes.StartController.start()
      )
    }

    "user enters a valid paid and claim amount on the MRN journey" in {
      val claim = sample[Claim]
        .copy(
          claimAmount = BigDecimal(1.00).setScale(2),
          paidAmount = BigDecimal(10.00).setScale(2),
          isFilled = false,
          taxCode = "A00"
        )

      val answers = ClaimsAnswer(claim)

      val session        = getSessionWithPreviousAnswer(Some(answers), None, None, getMRNAnswer())._1
      val updatedAnswer  = claim.copy(claimAmount = BigDecimal(5.00).setScale(2), isFilled = true)
      val updatedSession = updateSession(session, ClaimsAnswer(updatedAnswer))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockStoreSession(updatedSession)(Right(()))
      }

      checkIsRedirect(
        performAction(
          claim.id,
          Seq(
            "enter-claim" -> "5.00"
          )
        ),
        routes.EnterClaimController.checkClaim()
      )
    }

    "user enters a valid paid and claim amount on the Entry Number journey" in {
      val claim = sample[Claim]
        .copy(
          claimAmount = BigDecimal(1.00).setScale(2),
          paidAmount = BigDecimal(10.00).setScale(2),
          isFilled = false,
          taxCode = "A00"
        )

      val answers = ClaimsAnswer(claim)

      val session        = getSessionWithPreviousAnswer(Some(answers), None, None, getEntryNumberAnswer())._1
      val updatedAnswer  = claim.copy(claimAmount = BigDecimal(1.00).setScale(2), isFilled = true)
      val updatedSession = updateSession(session, ClaimsAnswer(updatedAnswer))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockStoreSession(updatedSession)(Right(()))
      }

      checkIsRedirect(
        performAction(
          claim.id,
          Seq(
            "enter-claim.paid-amount"  -> "10.00",
            "enter-claim.claim-amount" -> "5.00"
          )
        ),
        routes.EnterClaimController.checkClaim()
      )
    }

    "an invalid option value is submitted" in {
      val taxCode = TaxCode.A00
      val claim   = sample[Claim]
        .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = taxCode.value)

      val answers = ClaimsAnswer(claim)

      val session = getSessionWithPreviousAnswer(Some(answers), None, None, getEntryNumberAnswer())._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(
          claim.id,
          Seq(
            "enter-claim.paid-amount"  -> "sdfdf",
            "enter-claim.claim-amount" -> "dfsfs"
          )
        ),
        messageFromMessageKey("enter-claim.title", taxCode.value, "Customs Duty"),
        doc => {
          doc
            .select(".govuk-error-summary__list > li:nth-child(1) > a")
            .text() shouldBe messageFromMessageKey(
            s"enter-claim.paid-amount.error.invalid"
          )
          doc
            .select(".govuk-error-summary__list > li:nth-child(2) > a")
            .text() shouldBe messageFromMessageKey(
            s"enter-claim.claim-amount.error.invalid"
          )
        },
        BAD_REQUEST
      )
    }

  }

  "checkClaim page" must {

    def performAction(): Future[Result] = controller.checkClaim()(FakeRequest())

    "redirect to the start of the journey if the session is empty" in {
      val session = getSessionWithPreviousAnswer(None)._1
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

  "checkClaimSubmit page" must {

    def performAction(): Future[Result] = controller.checkClaimSubmit()(FakeRequest())

    "redirect to the start of the journey if the session is empty" in {
      val session = getSessionWithPreviousAnswer(None)._1
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

  "Entry Number Claim Amount Validation" must {
    val form        = EnterClaimController.entryClaimAmountForm
    val paidAmount  = "enter-claim.paid-amount"
    val claimAmount = "enter-claim.claim-amount"

    val goodData = Map(
      paidAmount  -> "99999999999.99",
      claimAmount -> "0.01"
    )

    "accept good declaration details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "paidAmount" should {
      "Accept shortest possible paidAmount" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(0, 2))).errors
        errors shouldBe Nil
      }
      "Accept longest possible paidAmount" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(11, 2))).errors
        errors shouldBe Nil
      }
      "Reject paidAmount decimals only too many" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(0, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("paid-amount.error.invalid")
      }
      "Reject paidAmount too many decimals" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(1, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("paid-amount.error.invalid")
      }
      "Reject paidAmount too long" in {
        val errors = form.bind(goodData.updated(paidAmount, moneyGen(12, 2))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("paid-amount.error.invalid")
      }
      "Reject negative numbers" in {
        val errors = form.bind(goodData.updated(paidAmount, "-1")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("paid-amount.error.invalid")
      }
    }

    "claimAmount" should {
      "Accept shortest possible claimAmount" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 2))).errors
        errors shouldBe Nil
      }
      "Accept longest possible claimAmount" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(11, 2))).errors
        errors shouldBe Nil
      }
      "Reject claimAmount decimals only too many" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject claimAmount too many decimals" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(1, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject claimAmount too long" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(12, 2))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject negative numbers" in {
        val errors = form.bind(goodData.updated(claimAmount, "-1")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject when claimAmount > paidAmount" in {
        val data   = Map(
          paidAmount  -> "100",
          claimAmount -> "101.00"
        )
        val errors = form.bind(data).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.claim")
      }
    }
  }

  "MRN Claim Amount Validation" must {
    val form        = EnterClaimController.mrnClaimAmountForm(BigDecimal("99999999999.99"))
    val claimAmount = "enter-claim"

    val goodData = Map(
      claimAmount -> "0.01"
    )

    "accept good declaration details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "claimAmount" should {
      "Accept shortest possible claimAmount" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 2))).errors
        errors shouldBe Nil
      }
      "Accept longest possible claimAmount" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(11, 2))).errors
        errors shouldBe Nil
      }
      "Reject claimAmount too many decimal digits" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(0, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject claimAmount too many decimals" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(1, 3))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject claimAmount too long" in {
        val errors = form.bind(goodData.updated(claimAmount, moneyGen(12, 2))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject negative numbers" in {
        val errors = form.bind(goodData.updated(claimAmount, "-1")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("claim-amount.error.invalid")
      }
      "Reject when claimAmount > paidAmount" in {
        val testForm = EnterClaimController.mrnClaimAmountForm(BigDecimal("100.00"))
        val data     = Map(claimAmount -> "101.00")
        val errors   = testForm.bind(data).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("invalid.claim")
      }
    }
  }
}
