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
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{BAD_REQUEST, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyBindable, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, DutiesSelectedAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{DisplayDeclaration, NdrcDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimsAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{moneyGen, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Duty, SessionData, SignedInUserDetails, _}

import java.util.UUID
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.{genListNdrcDetails, genNdrcDetails}

import scala.concurrent.Future
import scala.util.Random

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
class EnterClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  lazy val controller: EnterClaimController            = instanceOf[EnterClaimController]
  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  implicit val messagesApi: MessagesApi = controller.messagesApi

  implicit val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def createSessionWithPreviousAnswers(
    maybeClaimsAnswer: Option[ClaimsAnswer],
    maybeDutiesSelectedAnswer: Option[DutiesSelectedAnswer] = None,
    ndrcDetails: Option[List[NdrcDetails]] = None,
    movementReferenceNumber: MRN = sample[MRN]
  ): (SessionData, FillingOutClaim) = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val draftC285Claim      =
      generateDraftC285Claim(
        maybeClaimsAnswer,
        maybeDutiesSelectedAnswer,
        ndrcDetails,
        movementReferenceNumber
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
    movementReferenceNumber: MRN = sample[MRN]
  ): DraftClaim = {
    val acc14 = Functor[Id].map(sample[DisplayDeclaration])(dd =>
      dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = ndrcDetails))
    )

    DraftClaim.blank.copy(
      movementReferenceNumber = Some(movementReferenceNumber),
      claimsAnswer = maybeClaimsAnswer,
      dutiesSelectedAnswer = maybeDutiesSelectedAnswer,
      displayDeclaration = Some(acc14)
    )
  }

  private def updateSession(sessionData: SessionData, claimsAnswer: ClaimsAnswer): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, (draftClaim: DraftClaim))) =>
        val newClaim      = draftClaim.copy(claimsAnswer = Some(claimsAnswer))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                     => fail()
    }

  private def compareUrlsWithouthId(url1: String, url2: String): Any = {
    def removeId(in: String) = in.substring(0, in.lastIndexOf('/'))
    removeId(url1) shouldBe removeId(url2)
  }

  "startClaim page" must {

    def performAction(): Future[Result] = controller.startClaim()(FakeRequest())

    "redirect to the start of the journey if the session is empty" in {
      val session = createSessionWithPreviousAnswers(None)._1
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
      val session = createSessionWithPreviousAnswers(None, None)._1

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
      val session              = createSessionWithPreviousAnswers(Some(answers), dutiesSelectedAnswer)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkIsRedirect(
        performAction(),
        routes.EnterClaimController.checkClaimSummary()
      )

    }

    "Redirect to the enterClaim page if we have no claim for a duty" in {
      val taxCode              = TaxCode.A20
      val dutiesSelectedAnswer = DutiesSelectedAnswer(Duty(taxCode))
      val session              = createSessionWithPreviousAnswers(None, Some(dutiesSelectedAnswer))._1

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

      val session = createSessionWithPreviousAnswers(Some(ClaimsAnswer(claim)), Some(dutiesSelectedAnswer))._1

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
      val session = createSessionWithPreviousAnswers(None)._1
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session.copy(journeyStatus = None))
      }

      checkIsRedirect(
        performAction(UUID.randomUUID()),
        baseRoutes.StartController.start()
      )
    }

    "redirect to ineligible page when the claim id doesn't exist anymore" in {
      val session = createSessionWithPreviousAnswers(None)._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkIsRedirect(
        performAction(UUID.randomUUID()),
        baseRoutes.IneligibleController.ineligible()
      )
    }

    "render when the user has not answered this question before" in {
      val taxCode = TaxCode.B05
      val claim   = sample[Claim] //An answer is created by the startClaim method, with isFilled = false
        .copy(claimAmount = BigDecimal(0), paidAmount = BigDecimal(5), isFilled = false, taxCode = taxCode.value)
      val answers = ClaimsAnswer(claim)

      val session = createSessionWithPreviousAnswers(Some(answers))._1

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

      val session = createSessionWithPreviousAnswers(Some(answers))._1

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
      val session = createSessionWithPreviousAnswers(None)._1
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

      val session        =
        createSessionWithPreviousAnswers(Some(answers), None, None, sample[MRN])._1
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
        routes.EnterClaimController.checkClaimSummary()
      )
    }

    "an invalid option value is submitted" in {
      val taxCode = TaxCode.A00
      val claim   = sample[Claim]
        .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = taxCode.value)

      val answers = ClaimsAnswer(claim)

      val session =
        createSessionWithPreviousAnswers(Some(answers), None, None, sample[MRN])._1

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
        doc =>
          doc
            .select(".govuk-error-summary__list > li:nth-child(1) > a")
            .text() shouldBe messageFromMessageKey(
            s"enter-claim.error.required"
          ),
        BAD_REQUEST
      )
    }

  }

  "checkSummaryClaim page" must {

    def performAction(): Future[Result] = controller.checkClaimSummary()(FakeRequest())

    "redirect to the start of the journey if the session is empty" in {
      val session = createSessionWithPreviousAnswers(None)._1
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

  "checkClaimSummarySubmit page" must {

    def performAction(): Future[Result] = controller.checkClaimSummarySubmit()(FakeRequest())

    "redirect to the start of the journey if the session is empty" in {
      val session = createSessionWithPreviousAnswers(None)._1
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

  "checkClaimSummarySubmit page" must {

    def performAction(data: Seq[(String, String)]): Future[Result] =
      controller.checkClaimSummarySubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

    "Redirect to ReimbursementMethod if user says details are correct, they are on the MRN journey and all the selected duties are cma eligible" in {
      val taxCode = TaxCode.A00
      val claim   = sample[Claim]
        .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = taxCode.value)

      val selectedDuties = DutiesSelectedAnswer(Duty(taxCode))
      val ndrcDetails    = genNdrcDetails.sample.map(_.copy(taxType = taxCode.value, cmaEligible = Some("1"))).toList

      val answers = ClaimsAnswer(claim)

      val session =
        createSessionWithPreviousAnswers(
          Some(answers),
          Some(selectedDuties),
          Some(ndrcDetails),
          sample[MRN]
        )._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }
      val result = performAction(Seq(EnterClaimController.checkClaimSummaryKey -> "true"))
      checkIsRedirect(
        result,
        reimbursementRoutes.ReimbursementMethodController.showReimbursementMethod()
      )
    }

    "Redirect to CheckBankAccountDetails if user says details are correct and on the MRN journey" in {
      val taxCode = TaxCode.A00
      val claim   = sample[Claim]
        .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = taxCode.value)

      val answers = ClaimsAnswer(claim)

      val session =
        createSessionWithPreviousAnswers(
          Some(answers),
          None,
          None,
          sample[MRN]
        )._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }
      val result = performAction(Seq(EnterClaimController.checkClaimSummaryKey -> "true"))
      checkIsRedirect(
        result,
        routes.BankAccountController.checkBankAccountDetails(JourneyBindable.Single)
      )
    }

    "Redirect to SelectDuties if user says details are incorrect and on the MRN journey" in {

      val taxCode = TaxCode.A00
      val claim   = sample[Claim]
        .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = false, taxCode = taxCode.value)

      val answers = ClaimsAnswer(claim)

      val session =
        createSessionWithPreviousAnswers(
          Some(answers),
          None,
          None,
          sample[MRN]
        )._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }
      val result = performAction(Seq(EnterClaimController.checkClaimSummaryKey -> "false"))
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
          .copy(claimAmount = BigDecimal(10), paidAmount = BigDecimal(5), isFilled = true, taxCode = taxCode.value)
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

  "isCmaEligible" must {
    "Return true for a single selected duty with cma eligible" in {
      val ndrcDetail = genNdrcDetails.sample.map(_.copy(cmaEligible = Some("1"))).toList
      val claim      = generateDraftC285Claim(
        None,
        ndrcDetails = Some(ndrcDetail),
        maybeDutiesSelectedAnswer = Some(DutiesSelectedAnswer(Duty(TaxCode(ndrcDetail.head.taxType))))
      )
      EnterClaimController.isCmaEligible(claim) shouldBe true
    }

    "Return false for a single selected duty with cma not eligible" in {
      val ndrcDetail = genNdrcDetails.sample.map(_.copy(cmaEligible = Some("0"))).toList
      val claim      = generateDraftC285Claim(
        None,
        ndrcDetails = Some(ndrcDetail),
        maybeDutiesSelectedAnswer = Some(DutiesSelectedAnswer(Duty(TaxCode(ndrcDetail.head.taxType))))
      )
      EnterClaimController.isCmaEligible(claim) shouldBe false
    }

    "Return false for a single selected duty with cma not present" in {
      val ndrcDetail = genNdrcDetails.sample.map(_.copy(cmaEligible = None)).toList
      val claim      = generateDraftC285Claim(
        None,
        ndrcDetails = Some(ndrcDetail),
        maybeDutiesSelectedAnswer = Some(DutiesSelectedAnswer(Duty(TaxCode(ndrcDetail.head.taxType))))
      )
      println(ndrcDetail)
      EnterClaimController.isCmaEligible(claim) shouldBe false
    }

    "Return true for a multiple duties selected claim ith cma eligible on all" in {
      val ndrcDetails: List[NdrcDetails] =
        genListNdrcDetails().sample.map(_.map(_.copy(cmaEligible = Some("1")))).getOrElse(Nil)
      val selectedDuties                 = ndrcDetails.map(detail => Duty(TaxCode(detail.taxType)))
      val claim                          = generateDraftC285Claim(
        None,
        ndrcDetails = Some(ndrcDetails),
        maybeDutiesSelectedAnswer = Some(DutiesSelectedAnswer(selectedDuties).get)
      )
      EnterClaimController.isCmaEligible(claim) shouldBe true
    }

    "Return false for a multiple duties selected claim ith cma eligible on all except one" in {
      val initialNdrcDetails: List[NdrcDetails] = genListNdrcDetails().sample
        .map(
          _.map(_.copy(cmaEligible = Some("1")))
        )
        .getOrElse(Nil)
      val ineligible                            = Gen.pick(1, initialNdrcDetails.map(_.taxType)).sample.get.head
      val ndrcDetails                           = initialNdrcDetails.map {
        case detail if detail.taxType == ineligible => detail.copy(cmaEligible = Some("0"))
        case detail                                 => detail
      }
      val selectedDuties                        = ndrcDetails.map(detail => Duty(TaxCode(detail.taxType)))
      val claim                                 = generateDraftC285Claim(
        None,
        ndrcDetails = Some(ndrcDetails),
        maybeDutiesSelectedAnswer = Some(DutiesSelectedAnswer(selectedDuties).get)
      )
      EnterClaimController.isCmaEligible(claim) shouldBe false
    }
  }
}
