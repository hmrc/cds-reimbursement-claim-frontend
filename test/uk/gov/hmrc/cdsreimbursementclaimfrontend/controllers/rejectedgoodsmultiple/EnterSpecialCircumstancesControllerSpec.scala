/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import org.scalatest.BeforeAndAfterEach
import org.scalatest.EitherValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterSpecialCircumstancesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators.completeJourneyGenWithSpecialCircumstances
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class EnterSpecialCircumstancesControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with EitherValues {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterSpecialCircumstancesController = instanceOf[EnterSpecialCircumstancesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch  = instanceOf[FeatureSwitchService]
  private val messagesKey: String = "enter-special-circumstances.rejected-goods"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData.empty.copy(
    rejectedGoodsMultipleJourney = Some(RejectedGoodsMultipleJourney.empty(exampleEori))
  )

  def showPage(): Future[Result] =
    controller.show()(FakeRequest())

  def submitSpecialCircumstances(data: (String, String)*): Future[Result] =
    controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

  "Enter Special Circumstances Controller" must {
    "not find the page if rejected goods feature is disabled" in {
      featureSwitch.disable(Feature.RejectedGoods)
      status(showPage()) shouldBe NOT_FOUND
    }

    "display the page" when {
      "the user has not answered this question before" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          showPage(),
          messageFromMessageKey(s"$messagesKey.title")
        )
      }

      "display the page on a pre-existing journey" in forAll(completeJourneyGenWithSpecialCircumstances) { journey =>
        whenever(journey.answers.basisOfClaimSpecialCircumstances.isDefined) {
          val basisOFClaimSpecialCircumstances = journey.answers.basisOfClaimSpecialCircumstances.map(_.toString)
          val updatedSession                   = session.copy(rejectedGoodsMultipleJourney = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            showPage(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => selectedTextArea(doc) shouldBe basisOFClaimSpecialCircumstances
          )
        }
      }

    }

    "handle submit requests" when {
      "the user enters details for the first time" in {
        val journey        = RejectedGoodsMultipleJourney
          .empty(exampleEori)
          .submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
        val session        = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))
        val updatedJourney = journey
          .submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails)
          .getOrElse(fail("unable to get special circumstances"))
        val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          submitSpecialCircumstances(controller.formKey -> exampleSpecialCircumstancesDetails),
          routes.DisposalMethodController.show()
        )
      }

    }

    "redirect to CYA page" when {
      "journey is complete" in forAll(buildCompleteJourneyGen()) { journey =>
        val sessionWitJourney = session.copy(rejectedGoodsMultipleJourney =
          Some(journey.submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWitJourney)
          mockStoreSession(
            sessionWitJourney.copy(rejectedGoodsMultipleJourney =
              sessionWitJourney.rejectedGoodsMultipleJourney.map(
                _.submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails).value
              )
            )
          )(Right(()))
        }

        checkIsRedirect(
          submitSpecialCircumstances(controller.formKey -> exampleSpecialCircumstancesDetails),
          routes.CheckYourAnswersController.show()
        )
      }
    }

    "show an error summary" when {
      "the user submits empty details" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          submitSpecialCircumstances(controller.formKey -> ""),
          messageFromMessageKey(s"$messagesKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messagesKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "the user submits more than 500 characters" in {

        val answer = List.fill(600)('c').mkString(" ")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          submitSpecialCircumstances(controller.formKey -> answer),
          messageFromMessageKey(s"$messagesKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messagesKey.error.maxLength"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

    }
  }

  "Form Validation" must {
    val form     = enterSpecialCircumstancesForm
    val goodData = Map(
      messagesKey -> "A box of biscuits"
    )

    "accept special circumstances details" in {
      val errors = form.bind(goodData).errors
      errors shouldBe Nil
    }

    "special circumstances details" should {

      "Accept longest possible details" in {
        val errors = form.bind(goodData.updated(messagesKey, List.fill(500)("a").mkString(""))).errors
        errors shouldBe Nil
      }
      "Reject details when it's too long" in {
        val errors = form.bind(goodData.updated(messagesKey, List.fill(501)("a").mkString(""))).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.maxLength")
      }
    }
  }

}
