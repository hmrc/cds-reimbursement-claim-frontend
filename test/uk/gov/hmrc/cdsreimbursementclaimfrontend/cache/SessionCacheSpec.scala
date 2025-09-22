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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.cache

import com.typesafe.config.ConfigFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.test.Helpers.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCacheSpec.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.CdsVerifiedEmail
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.SessionId
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.test.CleanMongoCollectionSupport

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import uk.gov.hmrc.mongo.cache.DataKey
import play.api.libs.json.Reads
import uk.gov.hmrc.mongo.cache.CacheItem
import play.api.libs.json.Writes

class SessionCacheSpec
    extends AnyWordSpec
    with CleanMongoCollectionSupport
    with Matchers
    with Eventually
    with ScalaCheckDrivenPropertyChecks {

  val sessionStore =
    new DefaultSessionCache(mongoComponent, new CurrentTimestampSupport(), config)

  val errorSessionStore =
    new DefaultSessionCache(mongoComponent, new CurrentTimestampSupport(), config) {
      override def get()(implicit hc: HeaderCarrier): Future[Either[Error, Option[SessionData]]] =
        Future.successful(Left(Error("test")))

      override def store(sessionData: SessionData)(implicit hc: HeaderCarrier): Future[Either[Error, Unit]] =
        Future.successful(Left(Error("test")))
    }

  val failingSessionStore =
    new DefaultSessionCache(mongoComponent, new CurrentTimestampSupport(), config) {
      override def findById(cacheId: HeaderCarrier): Future[Option[CacheItem]] =
        Future.failed(new Exception("test"))

      override def put[A : Writes](cacheId: HeaderCarrier)(dataKey: DataKey[A], data: A): Future[CacheItem] =
        Future.failed(new Exception("test"))
    }

  val fatalSessionStore =
    new DefaultSessionCache(mongoComponent, new CurrentTimestampSupport(), config) {
      override def get[A : Reads](cacheId: HeaderCarrier)(dataKey: DataKey[A]): Future[Option[A]] =
        throw new Exception("test")

      override def put[A : Writes](cacheId: HeaderCarrier)(dataKey: DataKey[A], data: A): Future[CacheItem] =
        throw new Exception("test")

    }

  "SessionCache" must {

    "be able to insert SessionData into mongo and read it back" in new TestEnvironment {
      val sessionData = SessionData.empty
      val result      = sessionStore.store(sessionData)

      await(result) should be(Right(()))

      eventually {
        val getResult = sessionStore.get()
        await(getResult) should be(Right(Some(sessionData)))
      }

    }

    "return no SessionData if there is no data in mongo" in new TestEnvironment {
      await(sessionStore.get()) should be(Right(None))
    }

    "return error if cache get returns an error" in new TestEnvironment {
      await(errorSessionStore.get()) should be(Left(Error("test")))
    }

    "return error if cache store returns an error" in new TestEnvironment {
      await(errorSessionStore.store(SessionData.empty)) should be(Left(Error("test")))
    }

    "return error if cache fails to get session data" in new TestEnvironment {
      await(failingSessionStore.get()).isLeft should be(true)
    }

    "return error if cache fails to store session data" in new TestEnvironment {
      await(failingSessionStore.store(SessionData.empty)) should be(Right(()))
    }

    "return error if cache get access raises an exception" in new TestEnvironment {
      await(fatalSessionStore.get()) should be(Right(None))
    }

    "return error if cache store access raises an exception" in new TestEnvironment {
      await(fatalSessionStore.store(SessionData.empty)) should be(Right(()))
    }

    "return an error there is no session id in the header carrier" in {
      implicit val hc: HeaderCarrier = HeaderCarrier()
      val sessionData                = SessionData.empty

      await(sessionStore.store(sessionData)).isLeft shouldBe true
      await(sessionStore.get()).isLeft              shouldBe true
    }

    "update the session data" in new TestEnvironment {
      val sessionData         = SessionData.empty.copy(verifiedEmail =
        Some(CdsVerifiedEmail(address = "test@test.com", timestamp = "2021-01-01"))
      )
      val expectedSessionData =
        sessionData.copy(verifiedEmail = Some(CdsVerifiedEmail(address = "test2@test.com", timestamp = "2022-02-22")))
      await(sessionStore.store(sessionData))
      await(
        sessionStore.updateF(false)(session =>
          Future.successful(
            Right(
              session.copy(verifiedEmail = Some(CdsVerifiedEmail(address = "test2@test.com", timestamp = "2022-02-22")))
            )
          )
        )
      )                         should be(Right(expectedSessionData))
      await(sessionStore.get()) should be(Right(Some(expectedSessionData)))
    }

    "update the session data when identical session data returned" in new TestEnvironment {
      val sessionData = SessionData.empty.copy(verifiedEmail =
        Some(CdsVerifiedEmail(address = "test@test.com", timestamp = "2021-01-01"))
      )

      await(sessionStore.store(sessionData))

      await(
        sessionStore.updateF(false)(session => Future.successful(Right(session)))
      ) should be(Right(sessionData))

      await(sessionStore.get()) should be(Right(Some(sessionData)))
    }

    "update the session data with a forced session creation" in new TestEnvironment {

      val expectedSessionData =
        SessionData(verifiedEmail = Some(CdsVerifiedEmail(address = "test3@test.com", timestamp = "2022-02-22")))

      await(
        sessionStore.updateF(true)(session =>
          Future.successful(
            Right(
              session.copy(verifiedEmail = Some(CdsVerifiedEmail(address = "test3@test.com", timestamp = "2022-02-22")))
            )
          )
        )
      ) should be(Right(expectedSessionData))

      await(sessionStore.get()) should be(Right(Some(expectedSessionData)))
    }

    "do not update the session data if session does not exist" in new NoSessionTestEnvironment {
      val result = await(
        sessionStore.updateF(false)(session =>
          Future.successful(
            Right(SessionData.empty)
          )
        )
      )

      result.swap.map(_.message) should be(Right("Could not find sessionId"))
    }

    "do not update the session data if the update function returns an error" in new TestEnvironment {
      val sessionData = SessionData.empty.copy(verifiedEmail =
        Some(CdsVerifiedEmail(address = "test@test.com", timestamp = "2021-01-01"))
      )

      await(sessionStore.store(sessionData))

      await(
        sessionStore.updateF(false)(session =>
          Future.successful(
            Left(Error("test"))
          )
        )
      ) should be(Left(Error("test")))

      await(sessionStore.get()) should be(Right(Some(sessionData)))
    }

    "do not update the session data if the update function returns failed future" in new TestEnvironment {
      val sessionData = SessionData.empty.copy(verifiedEmail =
        Some(CdsVerifiedEmail(address = "test@test.com", timestamp = "2021-01-01"))
      )

      val exception = new Exception("test")

      await(sessionStore.store(sessionData))

      await(
        sessionStore.updateF(false)(session => Future.failed(exception))
      ) should be(Left(Error(exception)))

      await(sessionStore.get()) should be(Right(Some(sessionData)))
    }

    "do not update the session data if the update function throws an exception" in new TestEnvironment {
      val sessionData = SessionData.empty.copy(verifiedEmail =
        Some(CdsVerifiedEmail(address = "test@test.com", timestamp = "2021-01-01"))
      )

      val exception = new Exception("test")

      await(sessionStore.store(sessionData))

      await(
        sessionStore.updateF(false)(_ => throw exception)
      ) should be(Left(Error(exception)))

      await(sessionStore.get()) should be(Right(Some(sessionData)))
    }

    "do not update the session data if the update function returns an error (no session found)" in new TestEnvironment {
      await(
        sessionStore.updateF(false)(session =>
          Future.successful(
            Right(SessionData.empty)
          )
        )
      ) should be(Left(Error("no session found in mongo")))

      await(sessionStore.get()) should be(Right(None))
    }

    "do not update the session data if the update function returns an error (forced session creation)" in new TestEnvironment {
      await(
        sessionStore.updateF(true)(session =>
          Future.successful(
            Left(Error("test"))
          )
        )
      ) should be(Left(Error("test")))

      await(sessionStore.get()) should be(Right(None))
    }

    "do not update the session data if the update function returns failed future (forced session creation)" in new TestEnvironment {

      val exception = new Exception("test")

      await(
        sessionStore.updateF(true)(session => Future.failed(exception))
      ) should be(Left(Error(exception)))

      await(sessionStore.get()) should be(Right(None))
    }

    "do not update the session data if the update function throws an exception (forced session creation)" in new TestEnvironment {

      val exception = new Exception("test")

      await(
        sessionStore.updateF(true)(_ => throw exception)
      ) should be(Left(Error(exception)))

      await(sessionStore.get()) should be(Right(None))
    }

  }

}

object SessionCacheSpec {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        | session-store.expiry-time = 7 days
        |""".stripMargin
    )
  )

  class TestEnvironment {

    val sessionId = SessionId(UUID.randomUUID().toString)

    implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(sessionId))

  }

  class NoSessionTestEnvironment {

    implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = None)

  }

}
