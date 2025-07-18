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

import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature

class LandingPageSecuritiesControllerSpec extends ControllerSpec {

  private val controller: LandingPageSecuritiesController = instanceOf[LandingPageSecuritiesController]
  implicit val messagesApi: MessagesApi                   = controller.messagesApi
  private lazy val featureSwitch                          = instanceOf[FeatureSwitchService]

  "The Landing Page Controller" must {
    "redirect to securities GOV.UK landing page url" in {
      featureSwitch.enable(Feature.Securities)
      checkIsRedirect(
        controller.showLandingPageSecurities(FakeRequest()),
        viewConfig.govUkSecuritiesLandingPageUrl
      )
    }
  }
}
