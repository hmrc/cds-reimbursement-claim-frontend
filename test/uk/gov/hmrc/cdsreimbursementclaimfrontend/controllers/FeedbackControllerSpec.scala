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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

class FeedbackControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val config: Configuration = Configuration(
    ConfigFactory.parseString(
      """
        | self {
        |   url = host1.com
        |  },
        |  microservice {
        |    services {
        |     feedback-survey-frontend {
        |        protocol = http
        |        host = localhost
        |        port = 9514
        |     }
        |   }
        |}
        |""".stripMargin
    )
  )

  lazy val controller: FeedbackController = instanceOf[FeedbackController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  "Feedback controller" must {

    "handling requests to display the feedback page" must {

      "display the page" in {
        val eori: Eori = sample[Eori]

        val retrievedGGCredId = Credentials("gg", "GovernmentGateway")
        val ggCredId          = GGCredId(retrievedGGCredId.providerId)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              userType = Some(UserType.Organisation),
              journeyStatus = Some(
                FillingOutClaim(
                  ggCredId,
                  SignedInUserDetails(Some(Email("")), eori, Email("email"), ContactName("John Smith")),
                  DraftC285Claim.newDraftC285Claim
                )
              )
            )
          )
        }

        checkIsRedirect(
          controller.feedback()(FakeRequest()),
          "http://localhost:9514/feedback/CDSRC"
        )
      }

    }

  }

}
