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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.test

import java.net.URLEncoder

import org.mockito.ArgumentMatcher
import org.scalamock.scalatest.MockFactory
import play.api.mvc.{BodyParsers, Request}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.actions.AuthAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.model.{Eori, SignedInUser}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals._
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Name, Retrieval, ~}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

trait AuthenticationBehaviours extends MockFactory {
  self: ReimbursementSpec =>

  val EORI_LENGTH                                          = 12
  val mockAuthConnector                                    = mock[AuthConnector]
  val noBearerTokenMatcher: ArgumentMatcher[HeaderCarrier] = new ArgumentMatcher[HeaderCarrier] {
    def matches(item: HeaderCarrier): Boolean = item != null && item.authorization.isEmpty
  }
  val mockAuthAction                                       = new AuthAction(mockAuthConnector, appConfig, BodyParsers.utils.empty)

  def signedInScenario(test: SignedInUser => Any): Any =
    signedInScenario(newUser(randomEori))(test)

  def randomEori: Eori = Eori(Random.alphanumeric.take(EORI_LENGTH).mkString)

  def newUser(eori: Eori): SignedInUser = SignedInUser(
    Some(Credentials("2345235235", "GovernmentGateway")),
    Some(Name(Some("Aldo"), Some("Rain"))),
    Some("amina@hmrc.co.uk"),
    eori
  )

  def signedInScenario(user: SignedInUser)(test: SignedInUser => Any): Any = {
    (mockAuthConnector
      .authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
      .expects(*, credentials and name and email and allEnrolments, *, *)
      .returning(Future.successful(new ~(new ~(new ~(user.credentials, user.name), user.email), enrolments(user.eori))))
    test(user)
  }

  def enrolments(eori: Eori) = Enrolments(
    Set(
      Enrolment("IR-SA", List(EnrolmentIdentifier("UTR", "111111111")), "Activated", None),
      Enrolment("IR-CT", List(EnrolmentIdentifier("UTR", "222222222")), "Activated", None),
      Enrolment("HMRC-CUS-ORG", List(EnrolmentIdentifier("EORINumber", eori.number)), "Activated", None)
    )
  )

  def notSignedInScenario()(test: => Any): Any = {
    (mockAuthConnector
      .authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
      .expects(*, *, *, *)
      .returning(Future.failed(new NoActiveSession("A user is not logged in") {}))
    test
  }

  def expectedSignInRedirectPathFor(request: Request[_]): String = {
    val encodedContinueUrl   = URLEncoder.encode(request.uri, "UTF-8")
    val expectedRedirectPath = s"/gg/sign-in?continue=$encodedContinueUrl&origin=cds-reimbursement-claim-frontend"
    expectedRedirectPath
  }

}
