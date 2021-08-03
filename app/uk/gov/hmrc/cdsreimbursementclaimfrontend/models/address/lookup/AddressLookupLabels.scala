package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupLabels.ConfirmPageLabels

final case class AddressLookupLabels(confirmPageLabels: ConfirmPageLabels)

object AddressLookupLabels {

  final case class ConfirmPageLabels(infoSubheading: String)

  implicit val format: OFormat[AddressLookupLabels] = Json.format[AddressLookupLabels]
}
