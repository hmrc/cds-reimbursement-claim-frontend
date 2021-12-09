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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import org.scalatest.OptionValues
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.Future

class FeatureSwitchServiceSpec extends ControllerSpec with TableDrivenPropertyChecks with OptionValues {

  "FeatureSwitchService" should {
    val configuration = Configuration.from(Map("feature.bulk-claim" -> "abc"))
    val featureSwitch = new FeatureSwitchService(configuration)

    val featureList =
      Table(
        "Features",
        featureSwitch.NorthernIreland,
        featureSwitch.RejectedGoods
      )

    "enable and disable All features" in forAll(featureList) { feature =>
      feature.enable()
      featureSwitch.of(feature.name).value.isEnabled() shouldBe true
      feature.disable()
      featureSwitch.of(feature.name).value.isEnabled() shouldBe false
    }

    "Enable viewing of pages" in {
      val featureSwitch  = instanceOf[FeatureSwitchService]
      featureSwitch.RejectedGoods.enable()
      val testController =
        new TestController(featureSwitch)(instanceOf[MessagesControllerComponents], instanceOf[ErrorHandler])
      val result         = testController.test()(FakeRequest())
      status(result)          shouldBe 200
      contentAsString(result) shouldBe "ok"
    }

    "Disable viewing of pages" in {
      val featureSwitch  = instanceOf[FeatureSwitchService]
      featureSwitch.RejectedGoods.disable()
      val testController =
        new TestController(featureSwitch)(instanceOf[MessagesControllerComponents], instanceOf[ErrorHandler])
      val result         = testController.test()(FakeRequest())
      status(result) shouldBe 404
    }

  }

  class TestController(fs: FeatureSwitchService)(implicit
    val cc: MessagesControllerComponents,
    val errorHandler: ErrorHandler
  ) extends FrontendController(cc) {
    def test(): Action[AnyContent] = fs.RejectedGoods.hideIfNotEnabled async {
      Future.successful(Ok("ok"))
    }
  }

}
