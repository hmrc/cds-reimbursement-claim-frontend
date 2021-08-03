package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup

import play.api.libs.json.{Json, OFormat}

final case class InitiateAddressLookupRequest(
  options: AddressLookupOptions,
  labels: AddressLookupLabels,
  version: Int = 2
)

object InitiateAddressLookupRequest {
  implicit val format: OFormat[InitiateAddressLookupRequest] = Json.format[InitiateAddressLookupRequest]
}
