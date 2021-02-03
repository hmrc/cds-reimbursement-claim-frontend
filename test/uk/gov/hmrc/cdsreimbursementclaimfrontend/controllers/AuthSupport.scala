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

import play.api.Configuration
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.{EmptyPredicate, Predicate}
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Credentials, EmptyRetrieval, Retrieval, ~}
import uk.gov.hmrc.auth.core.syntax.retrieved.authSyntaxForRetrieved
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnrolmentConfig.EoriEnrolment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait AuthSupport {
  this: ControllerSpec with SessionSupport =>

  val mockAuthConnector: AuthConnector = mock[AuthConnector]

  lazy val testAuthenticatedAction = new AuthenticatedActionWithRetrievedData(
    mockAuthConnector,
    instanceOf[Configuration],
    instanceOf[ErrorHandler],
    mockSessionStore
  )(instanceOf[ExecutionContext])

  def mockAuth[R](predicate: Predicate, retrieval: Retrieval[R])(
    result: Future[R]
  ): Unit =
    (mockAuthConnector
      .authorise(_: Predicate, _: Retrieval[R])(
        _: HeaderCarrier,
        _: ExecutionContext
      ))
      .expects(predicate, retrieval, *, *)
      .returning(result)

  def mockAuthWithNoRetrievals(): Unit =
    mockAuth(EmptyPredicate, EmptyRetrieval)(Future.successful(()))

  val expectedRetrievals =
    Retrievals.affinityGroup and Retrievals.email and Retrievals.allEnrolments and Retrievals.credentials

  def mockAuthWithAllRetrievals(
    retrievedAffinityGroup: Option[AffinityGroup],
    retrievedEmail: Option[String],
    retrievedEnrolments: Set[Enrolment],
    retrievedCredentials: Option[Credentials]
  ): Unit =
    mockAuth(EmptyPredicate, expectedRetrievals)(
      Future successful (
        new ~(retrievedAffinityGroup, retrievedEmail) and Enrolments(retrievedEnrolments) and retrievedCredentials
      )
    )

  def mockAuthWithAllIndividualRetrievals(
    retrievedEmail: Option[String],
    retrievedCredentials: Credentials
  ): Unit =
    mockAuthWithAllRetrievals(
      Some(AffinityGroup.Individual),
      retrievedEmail,
      Set.empty,
      Some(retrievedCredentials)
    )

  def mockAuthWithEoriEnrolmentRetrievals(): Unit =
    mockAuthWithAllRetrievals(
      Some(AffinityGroup.Individual),
      None,
      Set(
        Enrolment(
          EoriEnrolment.key,
          Seq(
            EnrolmentIdentifier(
              EoriEnrolment.eoriEnrolmentIdentifier,
              "AB12345678901234Z"
            )
          ),
          ""
        )
      ),
      Some(Credentials("gg-cred-id", "GovernmentGateway"))
    )

}
