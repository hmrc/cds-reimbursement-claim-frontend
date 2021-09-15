package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JourneyBindableSpec extends AnyWordSpec with Matchers {
    "JourneyBindable" must {

      "Parse paths correctly" in {
        JourneyBindable.parse("single") shouldBe JourneyBindable.Single
        JourneyBindable.parse("scheduled") shouldBe JourneyBindable.Scheduled
        JourneyBindable.parse("bulk") shouldBe JourneyBindable.Bulk
      }

    }
}
