package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils.LanguageHelper._

class LanguageHelperSpec extends AnyWordSpec with Matchers {

  "LanguageHelper" should {
    "work when there are no journey keys / subKeys / level2 keys" in {
      lang("a", None, "b") shouldBe List("a.b")
    }

    "Return 2 sets of keys" in {
      lang("a", Some("b"), "c") shouldBe List("a.b.c", "a.c")
    }
  }
}
