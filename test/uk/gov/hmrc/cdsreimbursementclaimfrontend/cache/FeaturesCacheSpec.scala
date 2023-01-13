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
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.FeaturesCacheSpec.TestEnvironment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.FeaturesCacheSpec.config
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.SessionId
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.test.CleanMongoCollectionSupport

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global

class FeaturesCacheSpec
    extends AnyWordSpec
    with CleanMongoCollectionSupport
    with Matchers
    with Eventually
    with ScalaCheckDrivenPropertyChecks {

  val sessionStore =
    new DefaultFeaturesCache(mongoComponent, new CurrentTimestampSupport(), config)

  val featureSetGen: Gen[FeatureSet] =
    for {
      enabled  <- Gen.listOf(Gen.oneOf(Feature.values))
      disabled <- Gen.listOf(Gen.oneOf(Feature.values))
    } yield FeatureSet(enabled.toSet, disabled.toSet)

  "FeaturesCache" must {

    "be able to insert FeatureSet into mongo and read it back" in new TestEnvironment {
      forAll(featureSetGen) { featureSet: FeatureSet =>
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

}
