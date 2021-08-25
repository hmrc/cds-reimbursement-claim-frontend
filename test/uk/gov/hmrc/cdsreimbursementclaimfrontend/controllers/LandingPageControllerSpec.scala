package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import play.api.i18n.MessagesApi
import play.api.test.FakeRequest

class LandingPageControllerSpec extends ControllerSpec {

  private val controller: LandingPageController = instanceOf[LandingPageController]
  implicit val messagesApi: MessagesApi         = controller.messagesApi

  "Landing Page Controller" when {
    "displaying the landing page" must {
      checkPageIsDisplayed(
        controller.land()(FakeRequest()),
        messageFromMessageKey("landing.title")
      )
    }
  }
}
