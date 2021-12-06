package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.magnolia.Typeclass
import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.{genCountry, genPostcode}

object InspectionAddressGen {

  lazy val genInspectionAddress = arbitraryInspectionAddress.arbitrary

  implicit lazy val arbitraryInspectionAddress: Typeclass[InspectionAddress] =
      Arbitrary(
        for {
          num      <- Gen.choose(1, 100)
          street   <- genStringWithMaxSizeOfN(7)
          district <- Gen.option(genStringWithMaxSizeOfN(5))
          road     <- if (district.isDefined) Gen.option(genStringWithMaxSizeOfN(5)) else Gen.const(None)
          town     <- genStringWithMaxSizeOfN(10)
          postcode <- genPostcode
          country  <- genCountry
        } yield InspectionAddress(
          addressLine1 = s"$num $street",
          addressLine2 = district,
          addressLine3 = road,
          townOrCity = Some(town),
          postalCode= postcode
        )
      )

}
