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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import play.api.Configuration
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnrolmentConfig.EoriEnrolment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.TestFeatureSwitchService

trait AuthSupport {
  this: ControllerSpec with SessionSupport =>

  val mockAuthConnector: AuthConnector = mock[AuthConnector]

  lazy val testAuthenticatedAction = new AuthenticatedActionWithRetrievedData(
    mockAuthConnector,
    instanceOf[Configuration],
    instanceOf[ErrorHandler],
    mockSessionCache,
    new TestFeatureSwitchService()
  )(instanceOf[ExecutionContext])

  def mockAuth[R](predicate: Predicate, retrieval: Retrieval[R])(
    result: Future[R]
  ): Any =
    (mockAuthConnector
      .authorise(_: Predicate, _: Retrieval[R])(
        _: HeaderCarrier,
        _: ExecutionContext
      ))
      .expects(predicate, retrieval, *, *)
      .returning(result)

  def mockAuthWithNoRetrievals(): Any =
    mockAuth(EmptyPredicate, EmptyRetrieval)(Future.successful(()))

  val expectedRetrievals
    : Retrieval[Option[AffinityGroup] ~ Option[String] ~ Enrolments ~ Option[Credentials] ~ Option[Name]] =
    Retrievals.affinityGroup and Retrievals.email and Retrievals.allEnrolments and Retrievals.credentials and Retrievals.name

  def mockAuthWithAllRetrievals(
    retrievedAffinityGroup: Option[AffinityGroup],
    retrievedEmail: Option[String],
    retrievedEnrolments: Set[Enrolment],
    retrievedCredentials: Option[Credentials],
    retrievedName: Option[Name]
  ): Any =
    mockAuth(EmptyPredicate, expectedRetrievals)(
      Future successful (
        new ~(retrievedAffinityGroup, retrievedEmail) and Enrolments(
          retrievedEnrolments
        ) and retrievedCredentials and retrievedName
      )
    )

  def mockAuthWithAllIndividualRetrievals(
    retrievedEmail: Option[String],
    retrievedCredentials: Credentials,
    retrievedName: Name
  ): Any =
    mockAuthWithAllRetrievals(
      Some(AffinityGroup.Individual),
      retrievedEmail,
      Set.empty,
      Some(retrievedCredentials),
      Some(retrievedName)
    )

  def mockAuthWithEoriEnrolmentRetrievals(eori: Eori = Eori("AB12345678901234Z")): Any =
    mockAuthWithAllRetrievals(
      Some(AffinityGroup.Individual),
      None,
      Set(
        Enrolment(
          EoriEnrolment.key,
          Seq(
            EnrolmentIdentifier(
              EoriEnrolment.eoriEnrolmentIdentifier,
              eori.value
            )
          ),
          ""
        )
      ),
      Some(Credentials("gg-cred-id", "GovernmentGateway")),
      Some(Name(Some("John Smith"), Some("Smith")))
    )

  def mockAuthWithOrgWithEoriEnrolmentRetrievals(): Any =
    mockAuthWithAllRetrievals(
      Some(AffinityGroup.Organisation),
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
      Some(Credentials("gg-cred-id", "GovernmentGateway")),
      Some(Name(Some("John Smith"), Some("Smith")))
    )

  def mockAuthWithNonGGUserRetrievals(): Any =
    mockAuthWithAllRetrievals(
      None,
      None,
      Set.empty,
      Some(Credentials("id", "NonGG")),
      None
    )

}
