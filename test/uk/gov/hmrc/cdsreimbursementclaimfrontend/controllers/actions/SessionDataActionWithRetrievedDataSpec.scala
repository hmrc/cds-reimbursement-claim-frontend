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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions

import play.api.i18n.MessagesApi
import play.api.mvc.Results.Ok
import play.api.mvc.MessagesRequest
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Name
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SessionDataGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SessionDataActionWithRetrievedDataSpec extends ControllerSpec with SessionSupport {

  lazy val action: SessionDataActionWithRetrievedData =
    new SessionDataActionWithRetrievedData(
      mockSessionCache,
      instanceOf[ErrorHandler]
    )

  implicit lazy val messagesApi: MessagesApi = instanceOf[MessagesApi]

  "Session data action with retrieved data" must {

    lazy val messagesRequest =
      new MessagesRequest(FakeRequest(), messagesApi)

    lazy val authenticatedRequest =
      AuthenticatedRequestWithRetrievedData(
        RetrievedUserType.Individual(
          GGCredId("id"),
          Some(Email("email")),
          Eori("Some eori"),
          Some(Name(Some("John Smith"), Some("Smith")))
        ),
        Some(UserType.Individual),
        messagesRequest
      )

    val sessionData = sample[SessionData]

    def performAction(): Future[Result] =
      action.invokeBlock(
        authenticatedRequest,
        { r: RequestWithSessionDataAndRetrievedData[_] =>
          r.messagesApi shouldBe messagesRequest.messagesApi
          r.sessionData shouldBe sessionData
          Future.successful(Ok)
        }
      )

    "return an error if there is an error getting session data" in {
      mockGetSession(Left(Error(new Exception("boom!"))))
      checkIsTechnicalErrorPage(performAction())
    }

    "perform the action with the session data if it can be retrieved" in {
      mockGetSession(sessionData)
      status(performAction()) shouldBe OK
    }

  }

}
