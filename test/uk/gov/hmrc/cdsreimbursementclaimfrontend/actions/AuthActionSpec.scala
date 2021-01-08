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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.actions

import com.google.inject.Inject
import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty, MessagesControllerComponents}
import play.api.test.Helpers._
import play.mvc.Http.Status
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals.{credentials, _}
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Name, Retrieval, ~}
import uk.gov.hmrc.auth.core.{Enrolment, EnrolmentIdentifier, Enrolments}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.test.{AuthenticationBehaviours, ReimbursementSpec}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class AuthActionSpec extends { implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global }
with ReimbursementSpec with AuthenticationBehaviours {

  val authAction = new AuthAction[AnyContent](
    authConnector = mockAuthConnector,
    appConfig = appConfig,
    stubBodyParser(AnyContentAsEmpty)
  )

  val controller = new MyFakeController(authAction)

  "the action" should {

    "redirect to the Government Gateway sign-in page when no authenticated user" in notSignedInScenario() {
      val response = controller.dummyAction(req)
      status(response)           shouldBe (Status.SEE_OTHER)
      header(LOCATION, response) shouldBe Some(expectedSignInRedirectPathFor(req))
    }

    "proceed with wrapped request containing signed in user details when authenticated" in signedInScenario {
      someRegisteredUser =>
        val resp = controller.dummyAction(req)
        status(resp)          shouldBe (Status.OK)
        contentAsString(resp) shouldBe someRegisteredUser.eori.number
    }

    "redirect to not subscribed page when user is not subscribed to CDS" in {
      (mockAuthConnector
        .authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
        .expects(*, credentials and name and email and allEnrolments, *, *)
        .returning {
          val creds     = Some(Credentials("2345235235", "GovernmentGateway"))
          val names     = Some(Name(Some("Aldo"), Some("Rain")))
          val emails    = Some("amina@hmrc.co.uk")
          val enrolment =
            Enrolments(Set(Enrolment("IR-SA", List(EnrolmentIdentifier("UTR", "111111111")), "Activated", None)))
          Future.successful(new ~(new ~(new ~(creds, names), emails), enrolment))
        }

      val response = controller.dummyAction(req)
      status(response)                   shouldBe (Status.SEE_OTHER)
      header(LOCATION, response).foreach(_ should include("/not-subscribed-for-cds"))
    }
  }
}

class MyFakeController @Inject() (val authenticate: AuthAction[AnyContent])(implicit
  val mcc: MessagesControllerComponents
) extends FrontendController(mcc) {
  def dummyAction: Action[AnyContent] = authenticate { implicit req =>
    Ok(req.user.eori.number)
  }

}
