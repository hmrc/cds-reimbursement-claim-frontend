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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import org.scalatest.OptionValues
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.Configuration
import play.api.mvc.Action
import play.api.mvc.ActionBuilder
import play.api.mvc.ActionFilter
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.TestFeaturesCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.FeaturesCache
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import uk.gov.hmrc.http.SessionId
import java.util.UUID

class FeatureSwitchServiceSpec extends ControllerSpec with TableDrivenPropertyChecks with OptionValues {

  "FeatureSwitchService" should {
    val configuration =
      Configuration.from(Map("feature.bulk-claim" -> "abc"))

    val featureList   =
      Table[Feature](
        "Features",
        Feature.RejectedGoods,
        Feature.LimitedAccess,
        Feature.Overpayments_v2,
        Feature.ViewUpload
      )

    val featuresCache: FeaturesCache = new TestFeaturesCache

    implicit val hc: HeaderCarrier = new HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

    "enable and disable all features for test" in forAll(featureList) { feature =>
      val featureSwitch: FeatureSwitchService =
        new ConfiguredFeatureSwitchService(configuration, featuresCache)

      featureSwitch.enable(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe true
      featureSwitch.disable(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe false
      featureSwitch.enable(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe true
      featureSwitch.disable(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe false
    }

    "enable and disable all features for session" in forAll(featureList) { feature =>
      val featureSwitch: FeatureSwitchService =
        new ConfiguredFeatureSwitchService(configuration, featuresCache)

      featureSwitch.enableForSession(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe true
      featureSwitch.disableForSession(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe false
      featureSwitch.enableForSession(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe true
      featureSwitch.disableForSession(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe false
    }

    "enable and disable all features for test and session" in forAll(featureList) { feature =>
      val featureSwitch: FeatureSwitchService =
        new ConfiguredFeatureSwitchService(configuration, featuresCache)

      featureSwitch.enable(feature)
      featureSwitch.disableForSession(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe true
      featureSwitch.disable(feature)
      featureSwitch.enableForSession(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe false
      featureSwitch.disableForSession(feature)
      featureSwitch.enable(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe true
      featureSwitch.enableForSession(feature)
      featureSwitch.disable(feature)
      featureSwitch.isEnabled(Feature.of(feature.name).value) shouldBe false
    }

    "Enable viewing of pages" in {
      val featureSwitch  = instanceOf[FeatureSwitchService]
      featureSwitch.enable(Feature.RejectedGoods)
      val testController =
        new TestController(featureSwitch)(instanceOf[ErrorHandler], instanceOf[MessagesControllerComponents])
      val result         = testController.test()(FakeRequest())
      status(result)          shouldBe 200
      contentAsString(result) shouldBe "ok"
    }

    "Disable viewing of pages" in {
      val featureSwitch  = instanceOf[FeatureSwitchService]
      featureSwitch.disable(Feature.RejectedGoods)
      val testController =
        new TestController(featureSwitch)(instanceOf[ErrorHandler], instanceOf[MessagesControllerComponents])
      val result         = testController.test()(FakeRequest())
      status(result) shouldBe 404
    }

  }

  class TestController(fs: FeatureSwitchService)(implicit
    val errorHandler: ErrorHandler,
    override val controllerComponents: MessagesControllerComponents
  ) extends FrontendController(controllerComponents) {

    private def hideIfNotEnabled(feature: Feature): ActionBuilder[Request, AnyContent] with ActionFilter[Request] =
      new ActionBuilder[Request, AnyContent] with ActionFilter[Request] {

        def filter[A](input: Request[A]): Future[Option[Result]] = Future.successful {
          implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(input, input.session)
          if (fs.isEnabled(feature)) None
          else Some(NotFound(errorHandler.notFoundTemplate(input)))
        }

        override def parser: BodyParser[AnyContent] = controllerComponents.parsers.defaultBodyParser

        override protected def executionContext: ExecutionContext = controllerComponents.executionContext
      }

    def test(): Action[AnyContent] =
      hideIfNotEnabled(Feature.RejectedGoods) async {
        Future.successful(Ok("ok"))
      }
  }

}
