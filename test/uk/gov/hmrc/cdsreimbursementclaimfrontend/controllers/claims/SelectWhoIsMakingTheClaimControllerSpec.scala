package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec

class SelectWhoIsMakingTheClaimControllerSpec extends  ControllerSpec {

  "Form Validation" must {
    val form = SelectWhoIsMakingTheClaimController.chooseDeclarantTypeForm
    val claimKey = "select-who-is-making-the-claim"

    "accept Importer" in {
      val errors = form.bind(Map(claimKey -> "0")).errors
      errors shouldBe Nil
    }

    "accept Associated with Importer Company" in {
      val errors = form.bind(Map(claimKey -> "1")).errors
      errors shouldBe Nil
    }

    "accept Associated Representative Company" in {
      val errors = form.bind(Map(claimKey -> "2")).errors
      errors shouldBe Nil
    }

    "reject invalid numbers" in {
      val errors = form.bind(Map(claimKey -> "3")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("invalid")
    }

    "reject invalid chars" in {
      val errors = form.bind(Map(claimKey -> "a")).errors
      errors.headOption.getOrElse(fail()).messages shouldBe List("error.number")
    }


  }
}
