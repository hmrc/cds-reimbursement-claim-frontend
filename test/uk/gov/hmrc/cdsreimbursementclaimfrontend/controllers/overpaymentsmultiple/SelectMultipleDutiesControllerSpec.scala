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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

import cats.Functor
import cats.Id
import cats.data.NonEmptyList
import org.jsoup.nodes.Document
import org.scalacheck.Gen
import org.scalactic.source.Position
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectExciseValue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import scala.concurrent.Future
import scala.util.Random

class SelectMultipleDutiesControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: SelectMultipleDutiesController = instanceOf[SelectMultipleDutiesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  lazy val displayDeclaration: DisplayDeclaration = sample[DisplayDeclaration]
  lazy val nonEmptyListOfMRN: List[MRN]           = sample[List[MRN]](Gen.listOfN(20, genMRN))
  lazy val ndrc: NdrcDetails                      = sample[NdrcDetails]

  def randomListOfTaxCodes: List[TaxCode] = Random.shuffle(TaxCodes.excise).toList

  val nonEmptyListOfTaxCodesGen: Gen[List[TaxCode]]              =
    Gen.chooseNum(1, TaxCodes.excise.length).map(n => randomListOfTaxCodes.take(n))

  private def getSessionWithMRNs(
    selectedMrnIndex: Int,
    mrnCount: Int,
    taxCodes: List[TaxCode] = Nil,
    selectedTaxCodes: List[TaxCode] = Nil
  ): (SessionData, DraftClaim, Seq[MRN]) = {

    val leadMrn        = sample[MRN]
    val associatedMrns = nonEmptyListOfMRN.take(mrnCount - 1)

    val ndrcTaxCodeLists: Seq[List[TaxCode]] =
      (0 until mrnCount)
        .map(i =>
          if (i + 1 == selectedMrnIndex) taxCodes
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

    def acc14(index: Int) = Functor[Id].map(displayDeclaration) { dd =>
      val ndrsc = ndrcTaxCodeLists(index).map(code => ndrc.copy(taxType = code.value))
      dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrsc)))
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
      leadMrn :: associatedMrns
    )
  }
  def hasCheckbox(document: Document, taxCode: TaxCode): Boolean =
    !document
      .select(s"""input[value="${taxCode.value}"] """)
      .isEmpty

  def isChecked(document: Document, taxCode: TaxCode): Boolean =
    document
      .select(s"""input[value="${taxCode.value}"] """)
      .hasAttr("checked")

  def getBackLink(document: Document): String =
    document.select("a.govuk-back-link").attr("href")

  def performActionGetSelectDuties(i: Int): Future[Result] = controller.selectDuties(i)(FakeRequest())

  def getHintText(document: Document, hintTextId: String): Option[String] = {
    val hintTextElement = document.select(s"div#$hintTextId")

    if (hintTextElement.hasText) Some(hintTextElement.text()) else None
  }

  def assertAllRelevantDutiesAreRepresented(
    document: Document,
    taxCodes: List[TaxCode],
    selectedTaxCodes: List[TaxCode]
  )(implicit pos: Position): Unit = {
    taxCodes.foreach { taxCode =>
      hasCheckbox(document, taxCode) shouldBe true
      isChecked(document, taxCode)     should be(selectedTaxCodes.contains(taxCode))
    }
    randomListOfTaxCodes.toSet.diff(taxCodes.toSet).foreach { taxCode =>
      hasCheckbox(document, taxCode) shouldBe false
    }
  }

  def assertMrnIsDisplayedInBold(document: Document, mrn: MRN)(implicit pos: Position): Unit = {
    val element = document.select("span#MRN")
    element.text()        shouldBe mrn.value
    element.attr("class") shouldBe "govuk-!-font-weight-bold"
  }

  "Select Duties Controller" must {
    "redirect to the start of the journey" when {
      "there is no journey status in the session" in {
        (1 to 7).foreach { selectedMrnIndex =>
          val session = getSessionWithMRNs(selectedMrnIndex, mrnCount = 0)._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = None))
          }

          checkIsRedirect(
            performActionGetSelectDuties(selectedMrnIndex),
            baseRoutes.StartController.start()
          )
        }
      }
    }

    "display 'This MRN does not exist' error page" when {
      "an MRN has not been yet provided" in {
        (1 to 7).foreach { selectedMrnIndex =>
          val session = getSessionWithMRNs(selectedMrnIndex, mrnCount = selectedMrnIndex - 1)._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performActionGetSelectDuties(selectedMrnIndex),
            messageFromMessageKey("mrn-does-not-exist.title"),
            expectedStatus = Status.BAD_REQUEST
          )
        }
      }

    }

    "display the select duties page for the first MRN" when {
      "the user has provided first MRN and is selecting the duties for the first time" in {

        val taxCodes        = randomListOfTaxCodes.take(5)
        val (session, _, _) =
          getSessionWithMRNs(selectedMrnIndex = 1, mrnCount = 3, taxCodes = taxCodes, selectedTaxCodes = Nil)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performActionGetSelectDuties(1),
          messageFromMessageKey("multiple-select-duties.title", "first"),
          doc => assertAllRelevantDutiesAreRepresented(doc, taxCodes, Nil)
        )
      }

      "the user has provided first MRN and is changing previous selection of duties" in {

        val taxCodes         = randomListOfTaxCodes.take(5)
        val selectedTaxCodes = taxCodes.take(2)

        val (session, _, _) =
          getSessionWithMRNs(selectedMrnIndex = 1, mrnCount = 3, taxCodes, selectedTaxCodes)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performActionGetSelectDuties(1),
          messageFromMessageKey("multiple-select-duties.title", "first"),
          doc => assertAllRelevantDutiesAreRepresented(doc, taxCodes, selectedTaxCodes)
        )
      }
    }

    "display the select duties page for the second MRN" when {
      "the user has provided second MRN and is selecting the duties for the first time" in {

        val taxCodes        = randomListOfTaxCodes.take(2)
        val (session, _, _) =
          getSessionWithMRNs(selectedMrnIndex = 2, mrnCount = 3, taxCodes = taxCodes, selectedTaxCodes = Nil)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performActionGetSelectDuties(2),
          messageFromMessageKey("multiple-select-duties.title", "second"),
          doc => assertAllRelevantDutiesAreRepresented(doc, taxCodes, Nil)
        )
      }

      "the user has provided second MRN and is changing previous selection of duties" in {

        val taxCodes         = randomListOfTaxCodes.take(7)
        val selectedTaxCodes = taxCodes.take(5)

        val (session, _, _) =
          getSessionWithMRNs(selectedMrnIndex = 2, mrnCount = 3, taxCodes, selectedTaxCodes)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performActionGetSelectDuties(2),
          messageFromMessageKey("multiple-select-duties.title", "second"),
          doc => assertAllRelevantDutiesAreRepresented(doc, taxCodes, selectedTaxCodes)
        )
      }
    }

    "display the select duties page for further MRNs" when {
      "the user has provided a MRN and is selecting the duties for the first time" in {
        (3 to 13).foreach { selectedMrnIndex =>
          val taxCodes        = randomListOfTaxCodes.take(8)
          val (session, _, _) =
            getSessionWithMRNs(
              selectedMrnIndex,
              mrnCount = selectedMrnIndex + 2,
              taxCodes = taxCodes,
              selectedTaxCodes = Nil
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performActionGetSelectDuties(selectedMrnIndex),
            messageFromMessageKey("multiple-select-duties.title", OrdinalNumeral(selectedMrnIndex)),
            doc => assertAllRelevantDutiesAreRepresented(doc, taxCodes, Nil)
          )
        }
      }

      "the user has provided a MRN and is changing previous selection of duties" in {
        (3 to 13).foreach { selectedMrnIndex =>
          val taxCodes         = randomListOfTaxCodes.take(4)
          val selectedTaxCodes = taxCodes.take(2)

          val (session, _, _) =
            getSessionWithMRNs(selectedMrnIndex, selectedMrnIndex + 1, taxCodes, selectedTaxCodes)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performActionGetSelectDuties(selectedMrnIndex),
            messageFromMessageKey("multiple-select-duties.title", OrdinalNumeral(selectedMrnIndex)),
            doc => assertAllRelevantDutiesAreRepresented(doc, taxCodes, selectedTaxCodes)
          )
        }
      }
    }
  }

}
