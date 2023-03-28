/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import cats.Functor
import cats.Id
import org.jsoup.nodes.Document
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.SelectDutiesController.CmaEligibleAndDuties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectExciseValue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.PersonalEffects
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.A80
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.A85
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.A90
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.A95
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutiesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._

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

  val controller: SelectDutiesController = instanceOf[SelectDutiesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private def getSessionWithPreviousAnswer(
    maybeDutiesSelectedAnswer: Option[DutiesSelectedAnswer],
    movementReferenceNumber: MRN,
    displayDeclaration: Option[DisplayDeclaration] = None,
    basisOfClaim: BasisOfOverpaymentClaim = PersonalEffects
  ): (SessionData, FillingOutClaim) = {
    val draftC285Claim      = DraftClaim.blank.copy(
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

  def isA00Checked(document: Document): Boolean =
    isFieldValueChecked(document, TaxCode.A00.value)

  def isA30Checked(document: Document): Boolean =
    isFieldValueChecked(document, TaxCode.A30.value)

  def isA90Checked(document: Document): Boolean =
    isFieldValueChecked(document, TaxCode.A90.value)

  def isB00Checked(document: Document): Boolean =
    isFieldValueChecked(document, TaxCode.B00.value)

  def isFieldValueChecked(document: Document, fieldValue: String): Boolean =
    document.select(s"""input[value="$fieldValue"] """).hasAttr("checked")

  def getBackLink(document: Document): String =
    document.select("a.govuk-back-link").attr("href")

  def performAction(): Future[Result] = controller.selectDuties()(FakeRequest())

  def getHintText(document: Document, hintTextId: String) = {
    val hintTextElement = document.select(s"div#$hintTextId")

    if (hintTextElement.hasText) Some(hintTextElement.text()) else None
  }

  "Select Duties Controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        def performAction(): Future[Result] = controller.selectDuties()(FakeRequest())

        val session = getSessionWithPreviousAnswer(None, sample[MRN])._1

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

      "the user has answered this question before with a choice, but that choice is no longer available (e.g. Northern Ireland answer change)" in {
        val previousTaxCodes = Random.shuffle(TaxCodes.UK).take(3).toList
        val previousAnswer   = DutiesSelectedAnswer(previousTaxCodes.map(Duty(_))).getOrElse(fail())
        val newTaxCodes      = Random.shuffle(TaxCodes.excise).take(3)
        val ndrcs            = newTaxCodes.map(code => sample[NdrcDetails].copy(taxType = code.value)).toList
        val acc14            = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
        )
        val basisOfClaim     = IncorrectExciseValue

        val session =
          getSessionWithPreviousAnswer(
            Some(previousAnswer),
            sample[MRN],
            Some(acc14),
            basisOfClaim
          )._1

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

    "Available Duties" should {

      "Return Acc14 duties for an MRN" in {
        val taxCodes                              = Random.shuffle(TaxCodes.all).take(20).toList
        val ndrcs                                 = taxCodes.map(code => sample[NdrcDetails].copy(taxType = code.value))
        val acc14                                 = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
        )
        val session                               = getSessionWithPreviousAnswer(None, sample[MRN], Some(acc14))._2
        val dutiesAvailable: CmaEligibleAndDuties = SelectDutiesController.getAvailableDuties(session)
        dutiesAvailable.dutiesSelectedAnswer.map(_.toList) shouldBe Right(taxCodes.map(Duty(_)))
      }

      "Return Acc14 excise codes for an MRN when the Incorrect Excise code was selected previously" in {
        val basisOfClaim    = IncorrectExciseValue
        val taxCodes        = Random.shuffle(TaxCodes.excise).take(3).toList
        val ndrcs           = taxCodes.map(code => sample[NdrcDetails].copy(taxType = code.value))
        val acc14           = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
        )
        val session         = getSessionWithPreviousAnswer(None, sample[MRN], Some(acc14), basisOfClaim)._2
        val dutiesAvailable = SelectDutiesController.getAvailableDuties(session)
        dutiesAvailable.dutiesSelectedAnswer.map(_.toList) shouldBe Right(taxCodes.map(Duty(_)))
      }

    }

    "have CMA Eligible flag/Duties hint text" should {

      val taxCodes       = List(A80, A85, A90, A95)
      val testNdrcDetail = sample[NdrcDetails]

      val ndrcs: List[NdrcDetails] = List(
        testNdrcDetail.copy(taxType = A80.value, cmaEligible = Some("1")),
        testNdrcDetail.copy(taxType = A85.value, cmaEligible = Some("1")),
        testNdrcDetail.copy(taxType = A90.value, cmaEligible = Some("0")),
        testNdrcDetail.copy(taxType = A95.value, cmaEligible = None)
      )

      val acc14 = Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
      )

      "Acc14 excise code where the CMA eligible flag is true" in {

        val session = getSessionWithPreviousAnswer(None, sample[MRN], Some(acc14))._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val hintText =
          Some("This duty is not eligible for CMA repayment.")

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-duties.title"),
          doc => {
            getHintText(doc, "select-duties-item-hint")   shouldBe None
            getHintText(doc, "select-duties-2-item-hint") shouldBe None
            getHintText(doc, "select-duties-3-item-hint") shouldBe hintText
            getHintText(doc, "select-duties-4-item-hint") shouldBe hintText
          }
        )
      }

      "the CMA eligible flag indicates true" in {
        val session                               = getSessionWithPreviousAnswer(None, sample[MRN], Some(acc14))._2
        val dutiesAvailable: CmaEligibleAndDuties = SelectDutiesController.getAvailableDuties(session)
        dutiesAvailable.dutiesSelectedAnswer.map(_.toList) shouldBe Right(taxCodes.map(Duty(_)))

        val isCmaEligibleList: List[Boolean] = ndrcs.map(_.cmaEligible.getOrElse("0") === "1")

        dutiesAvailable.isCmaEligible shouldBe isCmaEligibleList
      }
    }

  }

}
