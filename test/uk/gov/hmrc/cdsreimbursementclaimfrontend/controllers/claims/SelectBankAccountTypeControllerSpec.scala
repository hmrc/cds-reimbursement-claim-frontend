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

import cats.implicits.catsSyntaxOptionId
import org.jsoup.nodes.Document
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.ContactName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import scala.collection.JavaConverters._
import scala.concurrent.Future

class SelectBankAccountTypeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with TableDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Multiple,
    JourneyBindable.Scheduled
  )

  val messageKey = "select-bank-account-type"

  lazy val errorHandler: ErrorHandler = instanceOf[ErrorHandler]

  lazy val controller: SelectBankAccountTypeController = instanceOf[SelectBankAccountTypeController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def performAction(journeyBindable: JourneyBindable): Future[Result] =
    controller.selectBankAccountType(journeyBindable)(FakeRequest())

  def performAction(journeyBindable: JourneyBindable, data: Seq[(String, String)]): Future[Result] =
    controller.selectBankAccountTypeSubmit(journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

  private def getSessionWithPreviousAnswer(
    bankAccountType: Option[BankAccountType],
    maybeTypeOfClaim: Option[TypeOfClaimAnswer]
  ): SessionData = {
    val draftC285Claim      = DraftClaim.blank.copy(
      bankAccountTypeAnswer = bankAccountType,
      typeOfClaim = maybeTypeOfClaim,
      movementReferenceNumber = Some(sample[MRN])
    )
    val ggCredId            = sample[GGCredId]
    val email               = sample[Email]
    val eori                = sample[Eori]
    val signedInUserDetails = SignedInUserDetails(Some(email), eori, email, ContactName("Anima Amina"))
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    SessionData.empty.copy(journeyStatus = Some(journey))
  }

  private def updateSession(sessionData: SessionData, bankAccountType: BankAccountType): SessionData =
    sessionData.journeyStatus match {
      case Some(FillingOutClaim(g, s, draftClaim: DraftClaim)) =>
        val newClaim      = draftClaim.copy(bankAccountTypeAnswer = Some(bankAccountType))
        val journeyStatus = FillingOutClaim(g, s, newClaim)
        sessionData.copy(journeyStatus = Some(journeyStatus))
      case _                                                   => fail()
    }

  def isBusinessChecked(document: Document): Boolean =
    isChecked(document, s"$messageKey-business-bank-account")

  def isPersonalChecked(document: Document): Boolean =
    isChecked(document, s"$messageKey-personal-bank-account")

  def isChecked(document: Document, fieldId: String): Boolean =
    document
      .getElementById(fieldId)
      .attributes()
      .asList()
      .asScala
      .map(_.getKey)
      .contains("checked")

  "SelectBankAccountTypeController" must {

    "redirect to the start of the journey" when {
      "there is no journey status in the session" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.selectBankAccountType(journey)(FakeRequest())

        val session = getSessionWithPreviousAnswer(None, None)

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

      "the user has not answered this question before" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(None, toTypeOfClaim(journey).some)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey),
          messageFromMessageKey(s"$messageKey.title"),
          doc => {
            isBusinessChecked(doc) shouldBe false
            isPersonalChecked(doc) shouldBe false
          }
        )
      }

      "the user has answered this question before and chosen Business Account " in forAll(journeys) { journey =>
        val session =
          getSessionWithPreviousAnswer(Some(BankAccountType.BusinessBankAccount), toTypeOfClaim(journey).some)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey),
          messageFromMessageKey(s"$messageKey.title"),
          doc => {
            isBusinessChecked(doc) shouldBe true
            isPersonalChecked(doc) shouldBe false
          }
        )
      }

      "the user has answered this question before and chosen Personal Account " in forAll(journeys) { journey =>
        val session =
          getSessionWithPreviousAnswer(Some(BankAccountType.PersonalBankAccount), toTypeOfClaim(journey).some)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey),
          messageFromMessageKey(s"$messageKey.title"),
          doc => {
            isBusinessChecked(doc) shouldBe false
            isPersonalChecked(doc) shouldBe true
          }
        )
      }
    }
    "handle submit requests" when {

      "user chooses the Business Account option" in forAll(journeys) { journey =>
        val session        = getSessionWithPreviousAnswer(None, toTypeOfClaim(journey).some)
        val updatedSession = updateSession(session, BankAccountType.BusinessBankAccount)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(journey, Seq(SelectBankAccountTypeController.selectBankAccountTypeKey -> "0")),
          routes.BankAccountController.enterBankAccountDetails(journey)
        )
      }

      "user chooses the Personal Account option" in forAll(journeys) { journey =>
        val session        = getSessionWithPreviousAnswer(None, toTypeOfClaim(journey).some)
        val updatedSession = updateSession(session, BankAccountType.PersonalBankAccount)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(journey, Seq(SelectBankAccountTypeController.selectBankAccountTypeKey -> "1")),
          routes.BankAccountController.enterBankAccountDetails(journey)
        )
      }

      "the user amends their previous answer" in forAll(journeys) { journey =>
        val session        =
          getSessionWithPreviousAnswer(Some(BankAccountType.BusinessBankAccount), toTypeOfClaim(journey).some)
        val updatedSession = updateSession(session, BankAccountType.PersonalBankAccount)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(journey, Seq(SelectBankAccountTypeController.selectBankAccountTypeKey -> "1")),
          routes.BankAccountController.enterBankAccountDetails(journey)
        )
      }

    }

    "show an error summary" when {
      "the user does not select an option and submits the page" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(None, toTypeOfClaim(journey).some)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journey, Seq.empty),
          messageFromMessageKey(s"$messageKey.title"),
          getErrorSummary(_) shouldBe messageFromMessageKey(s"$messageKey.error.required"),
          BAD_REQUEST
        )
      }

    }
  }

}
