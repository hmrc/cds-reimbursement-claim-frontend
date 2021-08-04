package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Arbitrary
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress

object ClaimGen {
  implicit val arbitraryMrnContactDetailsGen: Arbitrary[MrnContactDetails] = gen[MrnContactDetails]
  implicit val arbitraryNonUkAddressGen: Arbitrary[NonUkAddress]           = gen[NonUkAddress]
}
