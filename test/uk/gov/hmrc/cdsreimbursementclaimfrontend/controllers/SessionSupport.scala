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

import org.scalamock.handlers.CallHandler1
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait SessionSupport { this: MockFactory =>

  val mockSessionCache: SessionCache = mock[SessionCache]

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

  def mockStoreSession(
    session: SessionData
  )(result: Either[Error, Unit]): CallHandler2[SessionData, HeaderCarrier, Future[Either[Error, Unit]]] =
    (mockSessionCache
      .store(_: SessionData)(_: HeaderCarrier))
      .expects(session, *)
      .returning(Future.successful(result))

  def mockStoreSession(
    result: Either[Error, Unit]
  ): CallHandler2[SessionData, HeaderCarrier, Future[Either[Error, Unit]]] =
    (mockSessionCache
      .store(_: SessionData)(_: HeaderCarrier))
      .expects(*, *)
      .returning(Future.successful(result))
}
