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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import org.scalacheck.Gen
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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterSpecialCircumstancesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes

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

  private val messagesKey: String = "enter-special-circumstances.rejected-goods"

  val session: SessionData = SessionData.empty.copy(
    rejectedGoodsSingleClaim = Some(
      RejectedGoodsSingleClaim
        .empty(exampleDisplayDeclaration.getDeclarantEori)
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
        .getOrFail
    )
  )

  def showPage(): Future[Result] =
    controller.show(FakeRequest())

  def submitSpecialCircumstances(data: (String, String)*): Future[Result] =
    controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

  "Enter Special Circumstances Controller" must {

    "display the page" when {
      "the user has not answered this question before" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          showPage(),
          messageFromMessageKey(s"$messagesKey.title")
        )
      }

      "display the page on a pre-existing claim" in forAll(completeClaimGenWithSpecialCircumstances) { claim =>
        whenever(claim.answers.basisOfClaimSpecialCircumstances.isDefined) {
          val basisOFClaimSpecialCircumstances = claim.answers.basisOfClaimSpecialCircumstances
          val updatedSession                   = SessionData(claim)

          inSequence {
            mockAuthWithDefaultRetrievals()
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
        val claim: RejectedGoodsSingleClaim =
          RejectedGoodsSingleClaim
            .empty(exampleDisplayDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .map(_.submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances))
            .getOrFail

        val session        = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))
        val updatedClaim   = claim
          .submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails)
          .getOrElse(fail("unable to get special circumstances"))
        val updatedSession = SessionData(updatedClaim)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          submitSpecialCircumstances(controller.formKey -> exampleSpecialCircumstancesDetails),
          routes.DisposalMethodController.show
        )
      }

    }

    "redirect to CYA page" when {
      "claim is complete" in forAll(buildCompleteClaimGen()) { claim =>
        val sessionWitClaim = session.copy(rejectedGoodsSingleClaim =
          Some(claim.submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances))
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionWitClaim)
          mockStoreSession(
            sessionWitClaim.copy(rejectedGoodsSingleClaim =
              sessionWitClaim.rejectedGoodsSingleClaim.map(
                _.submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails).value
              )
            )
          )(Right(()))
        }

        checkIsRedirect(
          submitSpecialCircumstances(controller.formKey -> exampleSpecialCircumstancesDetails),
          routes.CheckYourAnswersController.show
        )
      }
    }

    "redirect to ineligible page" when {
      "basis of claim is not special circumstances" in {
        val claim: RejectedGoodsSingleClaim =
          RejectedGoodsSingleClaim
            .empty(exampleDisplayDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .map(_.submitBasisOfClaim(Gen.oneOf(BasisOfRejectedGoodsClaim.allButSpecialCircumstances).sample.get))
            .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          submitSpecialCircumstances(controller.formKey -> exampleSpecialCircumstancesDetails),
          baseRoutes.IneligibleController.ineligible
        )
      }
    }

    "show an error summary" when {
      "the user submits empty details" in {

        inSequence {
          mockAuthWithDefaultRetrievals()
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
          mockAuthWithDefaultRetrievals()
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
