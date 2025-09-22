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
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.test.Helpers.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.FeaturesCacheSpec.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.SessionId
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.test.CleanMongoCollectionSupport

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import uk.gov.hmrc.mongo.cache.CacheItem
import uk.gov.hmrc.mongo.cache.DataKey
import play.api.libs.json.Writes

class FeaturesCacheSpec
    extends AnyWordSpec
    with CleanMongoCollectionSupport
    with Matchers
    with Eventually
    with ScalaCheckDrivenPropertyChecks {

  val sessionStore =
    new DefaultFeaturesCache(mongoComponent, new CurrentTimestampSupport(), config)

  val errorSessionStore =
    new DefaultFeaturesCache(mongoComponent, new CurrentTimestampSupport(), config) {
      override def get()(implicit hc: HeaderCarrier): Future[Either[Error, FeatureSet]] =
        Future.successful(Left(Error("test")))

      override def store(featureSet: FeatureSet)(implicit hc: HeaderCarrier): Future[Either[Error, Unit]] =
        Future.successful(Left(Error("test")))
    }

  val failingSessionStore =
    new DefaultFeaturesCache(mongoComponent, new CurrentTimestampSupport(), config) {
      override def findById(cacheId: HeaderCarrier): Future[Option[CacheItem]] =
        Future.failed(new Exception("test"))

      override def put[A : Writes](cacheId: HeaderCarrier)(dataKey: DataKey[A], data: A): Future[CacheItem] =
        Future.failed(new Exception("test"))
    }

  val fatalSessionStore =
    new DefaultFeaturesCache(mongoComponent, new CurrentTimestampSupport(), config) {
      override def findById(cacheId: HeaderCarrier): Future[Option[CacheItem]] =
        throw new Exception("test")

      override def put[A : Writes](cacheId: HeaderCarrier)(dataKey: DataKey[A], data: A): Future[CacheItem] =
        throw new Exception("test")
    }

  val featureSetGen: Gen[FeatureSet] =
    for
      enabled  <- Gen.listOf(Gen.oneOf(Feature.values))
      disabled <- Gen.listOf(Gen.oneOf(Feature.values))
    yield FeatureSet(enabled.toSet, disabled.toSet)

  "FeaturesCache" must {

    "be able to insert FeatureSet into mongo and read it back" in new TestEnvironment {
      forAll(featureSetGen) { (featureSet: FeatureSet) =>
        val result = sessionStore.store(featureSet)

        await(result) should be(Right(()))

        eventually {
          val getResult = sessionStore.get()
          await(getResult) should be(Right(featureSet))
        }
      }
    }

    "return empty FeatureSet if there is no data in mongo" in new TestEnvironment {
      await(sessionStore.get()) should be(Right(FeatureSet.empty))
    }

    "return empty FeatureSet if there is no session id in the header carrier" in {
      implicit val hc: HeaderCarrier = HeaderCarrier()
      val featureSet                 = featureSetGen.sample.get

      await(sessionStore.store(featureSet)) shouldBe Right(())
      await(sessionStore.get())               should be(Right(FeatureSet.empty))
    }

    "return error if cache get returns an error" in new TestEnvironment {
      await(errorSessionStore.get()) should be(Left(Error("test")))
    }

    "return error if cache store returns an error" in new TestEnvironment {
      await(errorSessionStore.store(FeatureSet.empty)) should be(Left(Error("test")))
    }

    "return error if cache fails to get session data" in new TestEnvironment {
      await(failingSessionStore.get()).isLeft should be(true)
    }

    "return error if cache fails to store session data" in new TestEnvironment {
      await(failingSessionStore.store(FeatureSet.empty)) should be(Right(()))
    }

    "return error if cache get access raises an exception" in new TestEnvironment {
      await(fatalSessionStore.get()).isLeft should be(true)
    }

    "return error if cache store access raises an exception" in new TestEnvironment {
      await(fatalSessionStore.store(FeatureSet.empty)) should be(Right(()))
    }

    "be able to update the FeatureSet" in new TestEnvironment {

      val result = sessionStore.store(FeatureSet.empty)

      await(result) should be(Right(()))

      eventually {
        val updateResult = sessionStore.update(featureSet => featureSet.enable(Feature.NewEoriFormat))
        await(updateResult) should be(Right(()))

        eventually {
          val getResult = sessionStore.get()
          await(getResult) should be(Right(FeatureSet(enabled = Set(Feature.NewEoriFormat), disabled = Set.empty)))
        }
      }
    }

    "be able to update the FeatureSet and handle exception" in new TestEnvironment {

      val result    = sessionStore.store(FeatureSet.empty)
      val exception = new Exception("test")

      await(result) should be(Right(()))

      await(sessionStore.update(_ => throw exception)) should be(Left(Error(exception)))

      eventually {
        val getResult = sessionStore.get()
        await(getResult) should be(Right(FeatureSet.empty))
      }
    }

    "be able to update the FeatureSet if identical returned" in new TestEnvironment {

      val result = sessionStore.store(FeatureSet.empty)

      await(result) should be(Right(()))

      await(sessionStore.update(identity)) should be(Right(()))

      eventually {
        val getResult = sessionStore.get()
        await(getResult) should be(Right(FeatureSet.empty))
      }
    }

    "be able to update the FeatureSet if session not found" in new TestEnvironment {
      await(sessionStore.update(identity)) should be(Right(()))
    }

    "return an error if cache fails" in new NoSessionTestEnvironment {
      await(failingSessionStore.update(_ => FeatureSet(enabled = Set(Feature.NewEoriFormat)))).isLeft should be(true)
    }

    "return an error if cache fails fatally" in new NoSessionTestEnvironment {
      await(fatalSessionStore.update(_ => FeatureSet(enabled = Set(Feature.NewEoriFormat)))).isLeft should be(true)
    }

  }

}

object FeaturesCacheSpec {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        | features-store.expiry-time = 7 days
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
