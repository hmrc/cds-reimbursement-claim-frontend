package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.data.FormError

class DateInputHelperSpec  extends AnyWordSpec with Matchers {

  "DateInputHelper" should {
    "Show day error when day is not provided" in {
      println(DateInputHelper.withError("", Some(FormError("key", Seq("dayNot"))), "day"))
    }
  }
}
