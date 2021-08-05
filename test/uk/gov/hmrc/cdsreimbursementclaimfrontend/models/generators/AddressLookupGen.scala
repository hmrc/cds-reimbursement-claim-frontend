package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.magnolia.{Typeclass, gen}
import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.InitiateAddressLookupRequest

object AddressLookupGen {

  implicit val arbitraryInitiateAddressRequest: Typeclass[InitiateAddressLookupRequest] =
    gen[InitiateAddressLookupRequest]

  def genCountry: Gen[Country] =
    Gen.oneOf("GB", "LV", "SE", "DE", "NL", "IR", "NO", "DN").map(Country(_))

  def genPostcode: Gen[String] = for {
    first <- Gen.listOfN(3, Gen.alphaNumChar)
    last  <- Gen.listOfN(3, Gen.alphaNumChar)
  } yield s"${first.mkString("")} ${last.mkString("")}"

  implicit val arbitraryNonUkAddress: Typeclass[NonUkAddress] =
    Arbitrary(
      for {
        num      <- Gen.choose(1, 100)
        street   <- genStringWithMaxSizeOfN(7)
        district <- Gen.option(genStringWithMaxSizeOfN(5))
        road     <- if (district.isDefined) Gen.option(genStringWithMaxSizeOfN(5)) else Gen.const(None)
        town     <- genStringWithMaxSizeOfN(10)
        postcode <- genPostcode
        country  <- genCountry
      } yield NonUkAddress(
        line1 = s"$num $street",
        line2 = district,
        line3 = road,
        line4 = town,
        postcode = postcode,
        country = country
      )
    )
}
