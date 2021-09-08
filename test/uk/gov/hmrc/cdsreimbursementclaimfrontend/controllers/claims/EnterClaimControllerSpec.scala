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
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{BAD_REQUEST, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimController.{CheckClaimAnswer, ClaimAnswersAreCorrect, ClaimAnswersAreIncorrect, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, DutiesSelectedAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{DisplayDeclaration, NdrcDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimsAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{moneyGen, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, SignedInUserDetails, _}

import scala.concurrent.Future
import scala.util.Random

class EnterClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TableDrivenPropertyChecks {
  lazy val controller: EnterClaimController            = instanceOf[EnterClaimController]
  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def createSessionWithPreviousAnswers(
    maybeClaimsAnswer: Option[ClaimsAnswer],
    maybeDutiesSelectedAnswer: Option[DutiesSelectedAnswer] = None,
    ndrcDetails: Option[List[NdrcDetails]] = None,
    movementReferenceNumber: MovementReferenceNumber = getMRNAnswer(),
    checkClaimAnswer: Option[CheckClaimAnswer] = None
  ): (SessionData, FillingOutClaim) = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val draftC285Claim      =
      generateDraftC285Claim(
        maybeClaimsAnswer,
        maybeDutiesSelectedAnswer,
        ndrcDetails,
        movementReferenceNumber,
        checkClaimAnswer
      )
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey
    )
  }

  private def generateDraftC285Claim(
    maybeClaimsAnswer: Option[ClaimsAnswer],
    maybeDutiesSelectedAnswer: Option[DutiesSelectedAnswer] = None,
    ndrcDetails: Option[List[NdrcDetails]] = None,
    movementReferenceNumber: MovementReferenceNumber = getMRNAnswer(),
    checkClaimAnswer: Option[CheckClaimAnswer] = None
  ): DraftC285Claim = {
    val acc14 = Functor[Id].map(sample[DisplayDeclaration])(dd =>
      dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = ndrcDetails))
    )

    DraftC285Claim.newDraftC285Claim.copy(
      movementReferenceNumber = Some(movementReferenceNumber),
      claimsAnswer = maybeClaimsAnswer,
      dutiesSelectedAnswer = maybeDutiesSelectedAnswer,
      displayDeclaration = Some(acc14),
      checkClaimAnswer = checkClaimAnswer
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

  def getMRNAnswer(): MovementReferenceNumber = MovementReferenceNumber(Right(sample[MRN]))

  private def compareUrlsWithouthId(url1: String, url2: String): Any = {
    def removeId(in: String) = in.substring(0, in.lastIndexOf('/'))
    removeId(url1) shouldBe removeId(url2)
  }

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Bulk,
    JourneyBindable.Scheduled
  )

  "startClaim page" must {

    def performAction(journey: JourneyBindable): Future[Result] = controller.startClaim(journey)(FakeRequest())

    "redirect to the start of the journey if the session is empty" in forAll(journeys) { journey =>
      val session = createSessionWithPreviousAnswers(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(journey),
        baseRoutes.StartController.start()
      )
    }

    "Redirect to select duties if they were not selected previously" in forAll(journeys) { journey =>
      val session = createSessionWithPreviousAnswers(None, None)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkIsRedirect(
        performAction(journey),
        routes.SelectDutiesController.selectDuties()
      )
    }

    "redirect to the checkClaim page if we have finished claims for all duties" in forAll(journeys) { journey =>
      val selectedTaxCodes     = Random.shuffle(TaxCode.allTaxCodes).take(10)
      val dutiesSelectedAnswer = DutiesSelectedAnswer(selectedTaxCodes.map(Duty(_)))
      val claim                = selectedTaxCodes.map(taxCode =>
        sample[Claim]
          .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = true, taxCode = taxCode)
      )
      val answers              = ClaimsAnswer(claim).getOrElse(fail())
      val session              = createSessionWithPreviousAnswers(Some(answers), dutiesSelectedAnswer)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkIsRedirect(
        performAction(journey),
        routes.EnterClaimController.checkClaimSummary(journey)
      )

    }

    "Redirect to the enterClaim page if we have no claim for a duty" in forAll(journeys) { journey =>
      val taxCode              = TaxCode.A20
      val taxCategory          = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val dutiesSelectedAnswer = DutiesSelectedAnswer(Duty(taxCode))
      val session              = createSessionWithPreviousAnswers(None, Some(dutiesSelectedAnswer))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockStoreSession(Right(()))
      }

      val result = performAction(journey)

      status(result) shouldBe SEE_OTHER
      compareUrlsWithouthId(
        redirectLocation(result).getOrElse(fail()),
        routes.EnterClaimController.enterClaim(taxCategory, taxCode, journey).url
      )
    }

    "Redirect to the enterClaim page if we have an unfinished claim for a duty" in forAll(journeys) { journey =>
      val taxCode              = TaxCode.A20
      val taxCategory          = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val dutiesSelectedAnswer = DutiesSelectedAnswer(Duty(taxCode))
      val claim                = sample[Claim]
        .copy(
          claimAmount = BigDecimal(10),
          paidAmount = BigDecimal(5),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )

      val session = createSessionWithPreviousAnswers(Some(ClaimsAnswer(claim)), Some(dutiesSelectedAnswer))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val result = performAction(journey)

      status(result) shouldBe SEE_OTHER
      compareUrlsWithouthId(
        redirectLocation(result).getOrElse(fail()),
        routes.EnterClaimController.enterClaim(taxCategory, taxCode, journey).url
      )
    }
  }

  "enterClaim page on the MRN single journey" must {

    def performAction(taxCategory: TaxCategory, taxCode: TaxCode, journeyBindable: JourneyBindable): Future[Result] =
      controller.enterClaim(taxCategory, taxCode, journeyBindable)(FakeRequest())

    "redirect to the start of the journey if the session is empty" in {
      val journey     = JourneyBindable.Single
      val taxCode     = TaxCode.A20
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val session     = createSessionWithPreviousAnswers(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(taxCategory, taxCode, journey),
        baseRoutes.StartController.start()
      )
    }

    //TODO finish error page
    "render an error when the claim id doesn't exist anymore" in {
      val journey     = JourneyBindable.Single
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val session     = createSessionWithPreviousAnswers(None)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val result = performAction(taxCategory, taxCode, journey)
      status(result)          shouldBe OK
      contentAsString(result) shouldBe "This claim no longer exists"
    }

    "render when the user has not answered this question before" in {
      val journey     = JourneyBindable.Single
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim] //An answer is created by the startClaim method, with isFilled = false
        .copy(
          claimAmount = BigDecimal(0),
          paidAmount = BigDecimal(5),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )
      val answers     = ClaimsAnswer(claim)

      val session = createSessionWithPreviousAnswers(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(taxCategory, taxCode, journey),
        messageFromMessageKey(s"$singleLanguageKey.title", taxCode.value, "Value Added Tax")
      )
    }

    "render the previous answer when the user has answered this question before" in {
      val journey     = JourneyBindable.Single
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim]
        .copy(
          claimAmount = BigDecimal(10),
          paidAmount = BigDecimal(5),
          isFilled = true,
          taxCategory = taxCategory,
          taxCode = taxCode
        )
      val answers     = ClaimsAnswer(claim)

      val session = createSessionWithPreviousAnswers(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(taxCategory, taxCode, journey),
        messageFromMessageKey(s"$singleLanguageKey.title", taxCode.value, "Value Added Tax"),
        doc => doc.getElementById("enter-claim").`val`() shouldBe "10.00"
      )
    }

  }

  "enterClaim page on the MRN scheduled journey" must {

    def performAction(taxCategory: TaxCategory, taxCode: TaxCode, journeyBindable: JourneyBindable): Future[Result] =
      controller.enterClaim(taxCategory, taxCode, journeyBindable)(FakeRequest())

    "redirect to the start of the journey if the session is empty" in {
      val journey     = JourneyBindable.Scheduled
      val taxCode     = TaxCode.A20
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val session     = createSessionWithPreviousAnswers(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(taxCategory, taxCode, journey),
        baseRoutes.StartController.start()
      )
    }

    //TODO finish error page
    "render an error when the claim id doesn't exist anymore" in {
      val journey     = JourneyBindable.Scheduled
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val session     = createSessionWithPreviousAnswers(None)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val result = performAction(taxCategory, taxCode, journey)
      status(result)          shouldBe OK
      contentAsString(result) shouldBe "This claim no longer exists"
    }

    "render when the user has not answered this question before" in {
      val journey     = JourneyBindable.Scheduled
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim] //An answer is created by the startClaim method, with isFilled = false
        .copy(
          claimAmount = BigDecimal(0),
          paidAmount = BigDecimal(5),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )
      val answers     = ClaimsAnswer(claim)

      val session = createSessionWithPreviousAnswers(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(taxCategory, taxCode, journey),
        messageFromMessageKey(s"$scheduledLanguageKey.title", taxCode.value, "Value Added Tax")
      )
    }

    "render the previous answer when the user has answered this question before" in {
      val journey     = JourneyBindable.Scheduled
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim]
        .copy(
          claimAmount = BigDecimal(10),
          paidAmount = BigDecimal(5),
          isFilled = true,
          taxCategory = taxCategory,
          taxCode = taxCode
        )
      val answers     = ClaimsAnswer(claim)

      val session = createSessionWithPreviousAnswers(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(taxCategory, taxCode, journey),
        messageFromMessageKey(s"$scheduledLanguageKey.title", taxCode.value, "Value Added Tax"),
        doc => {
          doc.getElementById(s"$scheduledLanguageKey.paid-amount").`val`()  shouldBe "5.00"
          doc.getElementById(s"$scheduledLanguageKey.claim-amount").`val`() shouldBe "10.00"
        }
      )
    }

  }

  "enterClaim page on the MRN bulk-multi journey" must {

    def performAction(taxCategory: TaxCategory, taxCode: TaxCode, journeyBindable: JourneyBindable): Future[Result] =
      controller.enterClaim(taxCategory, taxCode, journeyBindable)(FakeRequest())

    "redirect to the start of the journey if the session is empty" in {
      val journey     = JourneyBindable.Bulk
      val taxCode     = TaxCode.A20
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val session     = createSessionWithPreviousAnswers(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(taxCategory, taxCode, journey),
        baseRoutes.StartController.start()
      )
    }

    //TODO finish error page
    "render an error when the claim id doesn't exist anymore" in {
      val journey     = JourneyBindable.Bulk
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val session     = createSessionWithPreviousAnswers(None)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      val result = performAction(taxCategory, taxCode, journey)
      status(result)          shouldBe OK
      contentAsString(result) shouldBe "This claim no longer exists"
    }

    "render when the user has not answered this question before" in {
      val journey     = JourneyBindable.Bulk
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim] //An answer is created by the startClaim method, with isFilled = false
        .copy(
          claimAmount = BigDecimal(0),
          paidAmount = BigDecimal(5),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )
      val answers     = ClaimsAnswer(claim)

      val session = createSessionWithPreviousAnswers(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(taxCategory, taxCode, journey),
        messageFromMessageKey(s"$singleLanguageKey.title", taxCode.value, "Value Added Tax")
      )
    }

    "render the previous answer when the user has answered this question before" in {
      val journey     = JourneyBindable.Bulk
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim]
        .copy(
          claimAmount = BigDecimal(10),
          paidAmount = BigDecimal(5),
          isFilled = true,
          taxCategory = taxCategory,
          taxCode = taxCode
        )
      val answers     = ClaimsAnswer(claim)

      val session = createSessionWithPreviousAnswers(Some(answers))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(taxCategory, taxCode, journey),
        messageFromMessageKey(s"$singleLanguageKey.title", taxCode.value, "Value Added Tax"),
        doc => doc.getElementById("enter-claim").`val`() shouldBe "10.00"
      )
    }

  }

  "enterClaimSubmit page on the MRN single journey" must {

    def performAction(
      taxCategory: TaxCategory,
      taxCode: TaxCode,
      journeyBindable: JourneyBindable,
      data: Seq[(String, String)]
    ): Future[Result] =
      controller.enterClaimSubmit(taxCategory, taxCode, journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

    "redirect to the start of the journey if the session is empty" in {
      val journey     = JourneyBindable.Single
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val session     = createSessionWithPreviousAnswers(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(taxCategory, taxCode, journey, Seq.empty),
        baseRoutes.StartController.start()
      )
    }

    "user enters a valid claim amount" in {
      val journey     = JourneyBindable.Single
      val taxCode     = TaxCode.A00
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim]
        .copy(
          claimAmount = BigDecimal(1.00).setScale(2),
          paidAmount = BigDecimal(10.00).setScale(2),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )

      val answers = ClaimsAnswer(claim)

      val session        = createSessionWithPreviousAnswers(Some(answers), None, None, getMRNAnswer(), None)._1
      val updatedAnswer  = claim.copy(claimAmount = BigDecimal(5.00).setScale(2), isFilled = true)
      val updatedSession = updateSession(session, ClaimsAnswer(updatedAnswer))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockStoreSession(updatedSession)(Right(()))
      }

      checkIsRedirect(
        performAction(
          taxCategory,
          taxCode,
          journey,
          Seq(singleLanguageKey -> "5.00")
        ),
        routes.EnterClaimController.checkClaimSummary(journey)
      )
    }

    "an invalid option value is submitted in" in {
      val journey     = JourneyBindable.Single
      val taxCode     = TaxCode.A00
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim]
        .copy(
          claimAmount = BigDecimal(10),
          paidAmount = BigDecimal(5),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )

      val answers = ClaimsAnswer(claim)

      val session = createSessionWithPreviousAnswers(Some(answers), None, None, getMRNAnswer(), None)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(
          taxCategory,
          taxCode,
          journey,
          Seq(singleLanguageKey -> "dfsfs")
        ),
        messageFromMessageKey("enter-claim.title", taxCode.value, "Customs Duty"),
        doc =>
          doc
            .select(".govuk-error-summary__list > li:nth-child(1) > a")
            .text() shouldBe messageFromMessageKey(
            s"enter-claim.claim-amount.error.invalid"
          ),
        BAD_REQUEST
      )
    }

  }

  "enterClaimSubmit page on the MRN scheduled journey" must {

    def performAction(
      taxCategory: TaxCategory,
      taxCode: TaxCode,
      journeyBindable: JourneyBindable,
      data: Seq[(String, String)]
    ): Future[Result] =
      controller.enterClaimSubmit(taxCategory, taxCode, journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

    "redirect to the start of the journey if the session is empty" in {
      val journey     = JourneyBindable.Scheduled
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val session     = createSessionWithPreviousAnswers(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(taxCategory, taxCode, journey, Seq.empty),
        baseRoutes.StartController.start()
      )
    }

    "user enters a valid paid and claim amount" in {
      val journey            = JourneyBindable.Scheduled
      val taxCode            = TaxCode.A00
      val taxCategory        = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val updatedClaimAmount = "5.00"
      val claim              = sample[Claim]
        .copy(
          claimAmount = BigDecimal(1.00).setScale(2),
          paidAmount = BigDecimal(10.00).setScale(2),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )

      val answers = ClaimsAnswer(claim)

      val session        = createSessionWithPreviousAnswers(Some(answers), None, None, getMRNAnswer(), None)._1
      val updatedAnswer  = claim.copy(claimAmount = BigDecimal(updatedClaimAmount).setScale(2), isFilled = true)
      val updatedSession = updateSession(session, ClaimsAnswer(updatedAnswer))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockStoreSession(updatedSession)(Right(()))
      }

      checkIsRedirect(
        performAction(
          taxCategory,
          taxCode,
          journey,
          Seq(
            s"$scheduledLanguageKey.paid-amount"  -> "10.00",
            s"$scheduledLanguageKey.claim-amount" -> updatedClaimAmount
          )
        ),
        routes.EnterClaimController.checkClaimSummary(journey)
      )
    }

    "an invalid option value is submitted" in {
      val journey     = JourneyBindable.Scheduled
      val taxCode     = TaxCode.A00
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim]
        .copy(
          claimAmount = BigDecimal(10),
          paidAmount = BigDecimal(5),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )

      val answers = ClaimsAnswer(claim)

      val session = createSessionWithPreviousAnswers(Some(answers), None, None, getMRNAnswer(), None)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(
          taxCategory,
          taxCode,
          journey,
          Seq(
            s"$scheduledLanguageKey.paid-amount"  -> "sdfdf",
            s"$scheduledLanguageKey.claim-amount" -> "dfsfs"
          )
        ),
        messageFromMessageKey(s"$scheduledLanguageKey.title", taxCode.value, "Customs Duty"),
        doc => {
          doc
            .select(".govuk-error-summary__list > li:nth-child(1) > a")
            .text() shouldBe messageFromMessageKey(
            s"$scheduledLanguageKey.paid-amount.error.invalid"
          )
          doc
            .select(".govuk-error-summary__list > li:nth-child(2) > a")
            .text() shouldBe messageFromMessageKey(
            s"$scheduledLanguageKey.claim-amount.error.invalid"
          )
        },
        BAD_REQUEST
      )
    }

  }

  "enterClaimSubmit page on the MRN bulk-multi journey" must {

    def performAction(
      taxCategory: TaxCategory,
      taxCode: TaxCode,
      journeyBindable: JourneyBindable,
      data: Seq[(String, String)]
    ): Future[Result] =
      controller.enterClaimSubmit(taxCategory, taxCode, journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

    "redirect to the start of the journey if the session is empty" in {
      val journey     = JourneyBindable.Bulk
      val taxCode     = TaxCode.B05
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val session     = createSessionWithPreviousAnswers(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(taxCategory, taxCode, journey, Seq.empty),
        baseRoutes.StartController.start()
      )
    }

    "user enters a valid paid and claim amount" in {
      val journey     = JourneyBindable.Bulk
      val taxCode     = TaxCode.A00
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim]
        .copy(
          claimAmount = BigDecimal(1.00).setScale(2),
          paidAmount = BigDecimal(10.00).setScale(2),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )

      val answers = ClaimsAnswer(claim)

      val session        = createSessionWithPreviousAnswers(Some(answers), None, None, getMRNAnswer(), None)._1
      val updatedAnswer  = claim.copy(claimAmount = BigDecimal(5.00).setScale(2), isFilled = true)
      val updatedSession = updateSession(session, ClaimsAnswer(updatedAnswer))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockStoreSession(updatedSession)(Right(()))
      }

      checkIsRedirect(
        performAction(
          taxCategory,
          taxCode,
          journey,
          Seq(
            "enter-claim" -> "5.00"
          )
        ),
        routes.EnterClaimController.checkClaimSummary(journey)
      )
    }

    "an invalid option value is submitted" in {
      val journey     = JourneyBindable.Bulk
      val taxCode     = TaxCode.A00
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim]
        .copy(
          claimAmount = BigDecimal(10),
          paidAmount = BigDecimal(5),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )

      val answers = ClaimsAnswer(claim)

      val session = createSessionWithPreviousAnswers(Some(answers), None, None, getMRNAnswer(), None)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(
          taxCategory,
          taxCode,
          journey,
          Seq(
            singleLanguageKey -> "dfsfs"
          )
        ),
        messageFromMessageKey("enter-claim.title", taxCode.value, "Customs Duty"),
        doc =>
          doc
            .select(".govuk-error-summary__list > li:nth-child(1) > a")
            .text() shouldBe messageFromMessageKey(
            s"enter-claim.claim-amount.error.invalid"
          ),
        BAD_REQUEST
      )
    }

  }

  "checkSummaryClaim page" must {

    def performAction(journey: JourneyBindable): Future[Result] = controller.checkClaimSummary(journey)(FakeRequest())

    "redirect to the start of the journey if the session is empty" in forAll(journeys) { journey =>
      val session = createSessionWithPreviousAnswers(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(journey),
        baseRoutes.StartController.start()
      )
    }
  }

  "checkClaimSummarySubmit page" must {

    def performAction(journey: JourneyBindable): Future[Result] =
      controller.checkClaimSummarySubmit(journey)(FakeRequest())

    "redirect to the start of the journey if the session is empty" in forAll(journeys) { journey =>
      val session = createSessionWithPreviousAnswers(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(journey),
        baseRoutes.StartController.start()
      )
    }
  }

  "checkClaimSummarySubmit page" must {

    def performAction(journey: JourneyBindable, data: Seq[(String, String)]): Future[Result] =
      controller.checkClaimSummarySubmit(journey)(FakeRequest().withFormUrlEncodedBody(data: _*))

    "Redirect to CheckBankAccountDetails if user says details are correct and on the MRN journey" in forAll(journeys) {
      journey =>
        val taxCode     = TaxCode.A00
        val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
        val claim       = sample[Claim]
          .copy(
            claimAmount = BigDecimal(10),
            paidAmount = BigDecimal(5),
            isFilled = false,
            taxCategory = taxCategory,
            taxCode = taxCode
          )

        val answers = ClaimsAnswer(claim)

        val session =
          createSessionWithPreviousAnswers(Some(answers), None, None, getMRNAnswer(), Some(ClaimAnswersAreCorrect))._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result = performAction(journey, Seq(summaryLanguageKey -> "0"))
        checkIsRedirect(
          result,
          routes.BankAccountController.checkBankAccountDetails(JourneyBindable.Single)
        )
    }

    "Redirect to EnterBankAccountDetails if user says details are correct on the scheduled journey" in forAll(
      journeys
    ) { journey =>
      val taxCode     = TaxCode.A00
      val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
      val claim       = sample[Claim]
        .copy(
          claimAmount = BigDecimal(10),
          paidAmount = BigDecimal(5),
          isFilled = false,
          taxCategory = taxCategory,
          taxCode = taxCode
        )

      val answers = ClaimsAnswer(claim)

      val session =
        createSessionWithPreviousAnswers(
          Some(answers),
          None,
          None,
          getMRNAnswer(),
          Some(ClaimAnswersAreCorrect)
        )._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }
      val result = performAction(journey, Seq(EnterClaimController.summaryLanguageKey -> "0"))
      checkIsRedirect(
        result,
        routes.BankAccountController.checkBankAccountDetails(JourneyBindable.Single)
      )
    }

    "Redirect to SelectDuties if user says details are incorrect and on the MRN journey" in forAll(journeys) {
      journey =>
        val taxCode     = TaxCode.A00
        val taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail)
        val claim       = sample[Claim]
          .copy(
            claimAmount = BigDecimal(10),
            paidAmount = BigDecimal(5),
            isFilled = false,
            taxCategory = taxCategory,
            taxCode = taxCode
          )

        val answers = ClaimsAnswer(claim)

        val session =
          createSessionWithPreviousAnswers(
            Some(answers),
            None,
            None,
            getMRNAnswer(),
            Some(ClaimAnswersAreIncorrect)
          )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result = performAction(journey, Seq(EnterClaimController.summaryLanguageKey -> "1"))
        checkIsRedirect(
          result,
          routes.SelectDutiesController.selectDuties()
        )
    }
  }

  "generateClaimsFromDuties" must {

    "Return an error if there are no duties" in {
      val draftC285Claim = generateDraftC285Claim(None)
      EnterClaimController.generateClaimsFromDuties(draftC285Claim) shouldBe Left(
        Error("No duties in session when arriving on ClaimController")
      )
    }

    "Return previous claims from the session" in {
      val selectedTaxCodes     = Random.shuffle(TaxCode.allTaxCodes).take(1)
      val dutiesSelectedAnswer = DutiesSelectedAnswer(selectedTaxCodes.map(Duty(_)))
      val claims               = selectedTaxCodes.map(taxCode =>
        sample[Claim]
          .copy(
            claimAmount = BigDecimal(10),
            paidAmount = BigDecimal(5),
            isFilled = true,
            taxCategory = TaxCategory.taxCodeToCategory.get(taxCode).getOrElse(fail),
            taxCode = taxCode
          )
      )
      val claimAnswers         = ClaimsAnswer(claims)

      val draftC285Claim = generateDraftC285Claim(claimAnswers, dutiesSelectedAnswer)
      EnterClaimController.generateClaimsFromDuties(draftC285Claim) shouldBe Right(claims)
    }

    "Generate new claims from duties" in {
      val numberOfDuties       = 10
      val selectedTaxCodes     = Random.shuffle(TaxCode.allTaxCodes).take(numberOfDuties)
      val dutiesSelectedAnswer = DutiesSelectedAnswer(selectedTaxCodes.map(Duty(_)))
      val draftC285Claim       = generateDraftC285Claim(None, dutiesSelectedAnswer)
      EnterClaimController.generateClaimsFromDuties(draftC285Claim).getOrElse(fail).size shouldBe numberOfDuties
    }
  }

  "Scheduled Claim Amount Validation" must {
    val form        = EnterClaimController.scheduledClaimAmountForm
    val paidAmount  = s"$scheduledLanguageKey.paid-amount"
    val claimAmount = s"$scheduledLanguageKey.claim-amount"

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
