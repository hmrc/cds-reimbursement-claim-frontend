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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class ClaimInvalidTPI04ControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with OptionValues {

  val messagesKey: String = "error-claim-invalid-TPI04"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ClaimInvalidTPI04Controller = instanceOf[ClaimInvalidTPI04Controller]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.Securities)
    featureSwitch.disable(Feature.LimitedAccessSecurities)
  }

  def validateClaimInvalidPage(doc: Document, rfsOpt: Option[ReasonForSecurity]) = {

    val startNewClaim = doc.getElementById("start-new-claim")
    startNewClaim.text()       shouldBe "start a new claim with a different MRN"
    startNewClaim.attr("href") shouldBe "/claim-back-import-duty-vat/securities/enter-movement-reference-number"

    val contactHmrc = doc.getElementById("contact-hmrc")

    if rfsOpt.exists(rfs => ReasonForSecurity.ntas.contains(rfs)) then
      contactHmrc.text()       shouldBe "ntis@hmrc.gov.uk"
      contactHmrc.attr("href") shouldBe "mailto:ntis@hmrc.gov.uk"
    else if rfsOpt.exists(rfs => ReasonForSecurity.niru.contains(rfs)) then
      contactHmrc.text() shouldBe "contact HMRC"
      contactHmrc.attr(
        "href"
      )                  shouldBe "https://www.gov.uk/government/organisations/hm-revenue-customs/contact/national-imports-reliefs-unit"
    else
      contactHmrc.text()       shouldBe "customsaccountingrepayments@hmrc.gov.uk"
      contactHmrc.attr("href") shouldBe "mailto:customsaccountingrepayments@hmrc.gov.uk"

  }

  "Invalid Claim Controller" when {
    "Show Error Invalid Claim page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "Display the page" in {
        forAll(completeJourneyGen) { journey =>
          val session = SessionData(journey)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(session)
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateClaimInvalidPage(doc, journey.getReasonForSecurity)
          )
        }
      }
    }
  }
}
