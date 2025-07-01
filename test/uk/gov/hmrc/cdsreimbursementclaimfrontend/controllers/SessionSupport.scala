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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import munit.diff.Diff
import org.scalamock.function.FunctionAdapter2
import org.scalamock.handlers.CallHandler1
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.io.AnsiColor.*

trait SessionSupport { this: MockFactory =>

  lazy val mockSessionCache: SessionCache = mock[SessionCache]

  def mockGetSession(
    result: Either[Error, Option[SessionData]]
  ): CallHandler1[HeaderCarrier, Future[Either[Error, Option[SessionData]]]] =
    (mockSessionCache
      .get()(_: HeaderCarrier))
      .expects(*)
      .returning(Future.successful(result))

  def mockGetSession(session: SessionData): CallHandler1[HeaderCarrier, Future[Either[Error, Option[SessionData]]]] =
    (mockSessionCache
      .get()(_: HeaderCarrier))
      .expects(*)
      .returning(Future.successful(Right(Some(session))))

  def mockStoreSessionNotCalled: CallHandler2[SessionData, HeaderCarrier, Future[Either[Error, Unit]]] =
    (mockSessionCache
      .store(_: SessionData)(_: HeaderCarrier))
      .expects(*, *)
      .never()

  def sessionMatcher(expectedSession: SessionData) =
    new FunctionAdapter2[SessionData, HeaderCarrier, Boolean]({ case (actualSession, _) =>
      if actualSession != expectedSession then {
        val s1   = Json.prettyPrint(Json.toJson(expectedSession))
        val s2   = Json.prettyPrint(Json.toJson(actualSession))
        val diff = new Diff(s1, s2)
        println(diff.createReport(s"$RED_B${WHITE}Expected session (red) is not same as actual one (gray) $RESET"))
        false
      } else true
    })

  def mockStoreSession(
    session: SessionData
  )(result: Either[Error, Unit]): CallHandler2[SessionData, HeaderCarrier, Future[Either[Error, Unit]]] =
    (mockSessionCache
      .store(_: SessionData)(_: HeaderCarrier))
      .expects(sessionMatcher(session))
      .noMoreThanOnce()
      .returning(Future.successful(result))

  def sessionMatcherWithCustomComparator(expectedSession: SessionData, compare: (SessionData, SessionData) => Boolean) =
    new FunctionAdapter2[SessionData, HeaderCarrier, Boolean]({ case (actualSession, _) =>
      if !compare(actualSession, expectedSession) then {
        val s1   = Json.prettyPrint(Json.toJson(expectedSession))
        val s2   = Json.prettyPrint(Json.toJson(actualSession))
        val diff = new Diff(s1, s2)
        println(diff.createReport(s"$RED_B${WHITE}Expected session (red) is not same as actual one (gray) $RESET"))
        false
      } else true
    })

  def mockStoreSessionWithCustomComparator(
    expectedSession: SessionData,
    compare: (SessionData, SessionData) => Boolean
  )(result: Either[Error, Unit]): CallHandler2[SessionData, HeaderCarrier, Future[Either[Error, Unit]]] =
    (mockSessionCache
      .store(_: SessionData)(_: HeaderCarrier))
      .expects(sessionMatcherWithCustomComparator(expectedSession, compare))
      .noMoreThanOnce()
      .returning(Future.successful(result))

  def mockStoreSession(
    result: Either[Error, Unit]
  ): CallHandler2[SessionData, HeaderCarrier, Future[Either[Error, Unit]]] =
    (mockSessionCache
      .store(_: SessionData)(_: HeaderCarrier))
      .expects(*, *)
      .noMoreThanOnce()
      .returning(Future.successful(result))

  def mockStoreSessions(): CallHandler2[SessionData, HeaderCarrier, Future[Either[Error, Unit]]] =
    (mockSessionCache
      .store(_: SessionData)(_: HeaderCarrier))
      .expects(*, *)
      .atLeastOnce()
      .returning(Future.successful(Right(())))

  lazy val compareOverpaymentsSingleFeatures: (SessionData, SessionData) => Boolean =
    (session1, session2) =>
      session1 == session2
        && (
          for {
            journey1 <- session1.overpaymentsSingleJourney
            journey2 <- session2.overpaymentsSingleJourney
          } yield (for {
            feature1 <- journey1.features
            feature2 <- journey2.features
          } yield feature1 == feature1)
            .getOrElse(journey1.features.isEmpty == journey2.features.isEmpty)
        )
          .getOrElse(false)

  lazy val compareOverpaymentsMultipleFeatures: (SessionData, SessionData) => Boolean =
    (session1, session2) =>
      session1 == session2
        && (
          for {
            journey1 <- session1.overpaymentsMultipleJourney
            journey2 <- session2.overpaymentsMultipleJourney
          } yield (for {
            feature1 <- journey1.features
            feature2 <- journey2.features
          } yield feature1 == feature1)
            .getOrElse(journey1.features.isEmpty == journey2.features.isEmpty)
        )
          .getOrElse(false)

  lazy val compareOverpaymentsScheduledFeatures: (SessionData, SessionData) => Boolean =
    (session1, session2) =>
      session1 == session2
        && (
          for {
            journey1 <- session1.overpaymentsScheduledJourney
            journey2 <- session2.overpaymentsScheduledJourney
          } yield (for {
            feature1 <- journey1.features
            feature2 <- journey2.features
          } yield feature1 == feature1)
            .getOrElse(journey1.features.isEmpty == journey2.features.isEmpty)
        )
          .getOrElse(false)

  lazy val compareRejectedGoodsSingleFeatures: (SessionData, SessionData) => Boolean =
    (session1, session2) =>
      session1 == session2
        && (
          for {
            journey1 <- session1.rejectedGoodsSingleJourney
            journey2 <- session2.rejectedGoodsSingleJourney
          } yield (for {
            feature1 <- journey1.features
            feature2 <- journey2.features
          } yield feature1 == feature1)
            .getOrElse(journey1.features.isEmpty == journey2.features.isEmpty)
        )
          .getOrElse(false)

  lazy val compareRejectedGoodsMultipleFeatures: (SessionData, SessionData) => Boolean =
    (session1, session2) =>
      session1 == session2
        && (
          for {
            journey1 <- session1.rejectedGoodsMultipleJourney
            journey2 <- session2.rejectedGoodsMultipleJourney
          } yield (for {
            feature1 <- journey1.features
            feature2 <- journey2.features
          } yield feature1 == feature1)
            .getOrElse(journey1.features.isEmpty == journey2.features.isEmpty)
        )
          .getOrElse(false)

  lazy val compareRejectedGoodsScheduledFeatures: (SessionData, SessionData) => Boolean =
    (session1, session2) =>
      session1 == session2
        && (
          for {
            journey1 <- session1.rejectedGoodsScheduledJourney
            journey2 <- session2.rejectedGoodsScheduledJourney
          } yield (for {
            feature1 <- journey1.features
            feature2 <- journey2.features
          } yield feature1 == feature1)
            .getOrElse(journey1.features.isEmpty == journey2.features.isEmpty)
        )
          .getOrElse(false)

  lazy val compareSecuritiesFeatures: (SessionData, SessionData) => Boolean =
    (session1, session2) =>
      session1 == session2
        && (
          for {
            journey1 <- session1.securitiesJourney
            journey2 <- session2.securitiesJourney
          } yield (for {
            feature1 <- journey1.features
            feature2 <- journey2.features
          } yield feature1 == feature1)
            .getOrElse(journey1.features.isEmpty == journey2.features.isEmpty)
        )
          .getOrElse(false)
}
