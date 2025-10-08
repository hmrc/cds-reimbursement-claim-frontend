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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import org.jsoup.nodes.Document
import org.scalacheck.ShrinkLowPriority
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.BeforeAndAfterEach
import org.scalatest.OptionValues
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class PartialClaimsControllerSpec
    extends PropertyBasedControllerSpec
    with SecuritiesClaimTestData
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TypeCheckedTripleEquals
    with OptionValues
    with ShrinkLowPriority
    with Logging {

  val partialClaimsKey: String = "partial-claims"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: PartialClaimsController = instanceOf[PartialClaimsController]
  implicit val messagesApi: MessagesApi   = controller.messagesApi
  implicit val messages: Messages         = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData(SecuritiesClaim.empty(exampleEori).submitMovementReferenceNumber(exampleMrn))

  private val incompleteClaim = buildClaimGen(
    acc14ConsigneeMatchesUserEori = true,
    submitBankAccountDetails = false,
    submitBankAccountType = false,
    submitDeclarantDetails = false
  ).map(
    _.fold(
      error =>
        throw new Exception(
          s"Cannot build complete SecuritiesClaim because of $error, fix the test data generator."
        ),
      identity
    )
  )

  def validatePartialClaimsPage(
    doc: Document,
    claim: SecuritiesClaim,
    isError: Boolean = false
  ) = {
    val title   = doc.select("title").first().text()
    val heading = doc.select(".govuk-heading-l").eachText().asScala.toList
    val legend  = doc.select(".govuk-fieldset__legend").eachText().asScala.toList

    title           should ===(
      "Partial claims - Claim back import duty and VAT - GOV.UK"
    )
    heading         should ===(
      List(
        "Partial claims"
      )
    )
    legend          should ===(List("Do you want to continue?"))
    radioItems(doc) should contain theSameElementsAs Seq(
      ("Yes", "true"),
      ("No", "false")
    )
    hasContinueButton(doc)
  }

  "Partial Claims Controller" when {
    "show page is called" must {
      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display the page on a complete claim" in
        forAll(buildCompleteClaimGen(numberOfSecurityDetails = Some(1))) { claim =>
          val updatedSession = SessionData.empty.copy(securitiesClaim = Some(claim))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("partial-claims.title"),
            doc => validatePartialClaimsPage(doc, claim)
          )
        }
    }

    "submit page is called" must {
      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "continue to select duties page when yes is selected" in {
        forAll(incompleteClaim) { claim =>
          val sessionData = SessionData(claim)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performAction(Seq(partialClaimsKey -> "true")),
            routes.SelectDutiesController.showFirst
          )
        }
      }

      "continue to claim deleted page when no is selected" in {
        forAll(incompleteClaim) { claim =>
          val sessionData = SessionData(claim)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performAction(Seq(partialClaimsKey -> "false")),
            routes.ClaimDeletedController.show
          )
        }
      }

      "display error when no option selected" in {
        forAll(incompleteClaim) { claim =>
          val updatedSession = SessionData.empty.copy(securitiesClaim = Some(claim))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageWithErrorIsDisplayed(
            performAction(Seq()),
            messageFromMessageKey("partial-claims.title"),
            messageFromMessageKey("partial-claims.error.required")
          )
        }
      }
    }
  }
}
