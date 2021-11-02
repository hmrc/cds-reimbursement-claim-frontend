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

import cats.Functor
import cats.Id
import cats.data.NonEmptyList
import org.jsoup.nodes.Document
import org.scalacheck.Gen
import org.scalactic.source.Position
import org.scalatest.Assertion
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaim.IncorrectExciseValue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps

import scala.concurrent.Future
import scala.util.Random
import play.api.http.Status

class EnterMultipleClaimsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterMultipleClaimsController = instanceOf[EnterMultipleClaimsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  lazy val displayDeclaration = sample[DisplayDeclaration]
  lazy val nonEmptyListOfMRN  = sample[List[MRN]](Gen.listOfN(20, genMRN))
  lazy val ndrc               = sample[NdrcDetails]

  def randomListOfTaxCodes = Random.shuffle(TaxCodes.excise).toList

  val nonEmptyListOfTaxCodesGen =
    Gen.chooseNum(1, TaxCodes.excise.length).map(n => randomListOfTaxCodes.take(n))

  private def getSessionWithSelectedDuties(
    selectedMrnIndex: Int,
    mrnCount: Int,
    selectedTaxCodes: List[TaxCode] = Nil
  ): (SessionData, DraftClaim, Seq[MRN], Seq[NdrcDetails]) = {

    val leadMrn        = sample[MRN]
    val associatedMrns = nonEmptyListOfMRN.take(mrnCount - 1)

    val ndrcTaxCodeLists: Seq[List[TaxCode]] =
      (0 until mrnCount)
        .map(i =>
          if (i + 1 == selectedMrnIndex) selectedTaxCodes
          else sample(nonEmptyListOfTaxCodesGen)
        )

    val selectedTaxCodeLists: Seq[List[TaxCode]] =
      selectedTaxCodes match {
        case Nil => Nil
        case _   =>
          ndrcTaxCodeLists.zipWithIndex.map { case (l, i) =>
            if (i + 1 == selectedMrnIndex) selectedTaxCodes else l.take(i + 1)
          }
      }

    val selectedDutiesLists: List[List[Duty]] =
      selectedTaxCodeLists.map(_.map(Duty.apply)).toList

    def ndrscDetails(index: Int): List[NdrcDetails] =
      ndrcTaxCodeLists(index).map(code => ndrc.copy(taxType = code.value))

    def acc14(index: Int) = Functor[Id].map(displayDeclaration) { dd =>
      dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrscDetails(index))))
    }

    val draftC285Claim = DraftClaim.blank.copy(
      movementReferenceNumber = if (mrnCount > 0) Some(leadMrn) else None,
      displayDeclaration = if (mrnCount > 0) Some(acc14(0)) else None,
      associatedMRNsAnswer = NonEmptyList.fromList(associatedMrns),
      associatedMRNsDeclarationAnswer = NonEmptyList.fromList(associatedMrns.zipWithIndex.map(x => acc14(x._2 + 1))),
      basisOfClaimAnswer = Some(IncorrectExciseValue),
      dutiesSelectedAnswer = selectedDutiesLists.headOption.flatMap(NonEmptyList.fromList),
      associatedMRNsDutiesSelectedAnswer =
        NonEmptyList.fromList(selectedDutiesLists.drop(1).map(NonEmptyList.fromListUnsafe))
    )

    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]

    val journey = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)

    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey.draftClaim,
      leadMrn :: associatedMrns,
      Seq.empty
    )
  }

  def getBackLink(document: Document): String =
    document.select("a.govuk-back-link").attr("href")

  def performActionEnterClaim(i: Int, taxCode: TaxCode): Future[Result] =
    controller.enterClaim(i, taxCode)(FakeRequest())

  def getHintText(document: Document, hintTextId: String): Option[String] = {
    val hintTextElement = document.select(s"div#$hintTextId")

    if (hintTextElement.hasText) Some(hintTextElement.text()) else None
  }

  def assertHintTextIsDisplayed(
    document: Document,
    expectedHintText: String
  )(implicit pos: Position): Assertion =
    getHintText(document, "multiple-enter-claim-hint") shouldBe Some(expectedHintText)

  def assertMrnIsDisplayedInBold(document: Document, mrn: MRN)(implicit pos: Position): Assertion = {
    val element = document.select("span#MRN")
    element.text()        shouldBe mrn.value
    element.attr("class") shouldBe "govuk-!-font-weight-bold"
  }

  def assertClaimAmountIsDisplayed(document: Document, expectedAmount: Option[String])(implicit pos: Position): Unit = {
    val element = document.select("span#paid-amount")
    expectedAmount.foreach(amount =>
      element.text() should startWith(messageFromMessageKey("multiple-enter-claim.paid-amount-label", amount))
    )
  }

  "Enter Multiple Claims Controller" must {
    "redirect to the start of the journey" when {
      "there is no journey status in the session" in {
        (1 to 7).foreach { selectedMrnIndex =>
          val session = getSessionWithSelectedDuties(selectedMrnIndex, mrnCount = 0)._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = None))
          }

          checkIsRedirect(
            performActionEnterClaim(selectedMrnIndex, TaxCode.A00),
            baseRoutes.StartController.start()
          )
        }
      }
    }

    "display 'This MRN does not exist' error page" when {
      "an MRN has not been yet provided" in {
        (1 to 7).foreach { selectedMrnIndex =>
          val session = getSessionWithSelectedDuties(selectedMrnIndex, mrnCount = selectedMrnIndex - 1)._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performActionEnterClaim(selectedMrnIndex, TaxCode.A00),
            messageFromMessageKey("mrn-does-not-exist.title"),
            expectedStatus = Status.BAD_REQUEST
          )
        }
      }

    }

    "display the enter claim page for the lead MRN and the tax code" when {
      (1 to 7).foreach { selectedMrnIndex =>
        s"the user has provided ${OrdinalNumeral(selectedMrnIndex)} MRN and has selected the duties" in {

          val selectedTaxCodes = randomListOfTaxCodes.take(selectedMrnIndex)

          val (session, _, mrns, ndrcDetails) =
            getSessionWithSelectedDuties(
              selectedMrnIndex,
              mrnCount = selectedMrnIndex + 1,
              selectedTaxCodes = selectedTaxCodes
            )

          selectedTaxCodes.foreach { taxCode =>
            val ndrc = ndrcDetails.find(_.taxType === taxCode)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }
            checkPageIsDisplayed(
              performActionEnterClaim(selectedMrnIndex, taxCode),
              messageFromMessageKey(
                "multiple-enter-claim.title",
                taxCode.value,
                messageFromMessageKey(s"select-duties.duty.$taxCode"),
                OrdinalNumeral(selectedMrnIndex)
              ),
              doc => {
                assertMrnIsDisplayedInBold(doc, mrns(selectedMrnIndex - 1))
                assertHintTextIsDisplayed(
                  doc,
                  messageFromMessageKey(
                    s"multiple-enter-claim.claim-amount.hint",
                    taxCode,
                    messageFromMessageKey(s"select-duties.duty.$taxCode")
                  )
                )
                assertClaimAmountIsDisplayed(doc, ndrc.map(n => BigDecimal(n.amount).toPoundSterlingString))
              }
            )
          }
        }
      }
    }
  }

}
