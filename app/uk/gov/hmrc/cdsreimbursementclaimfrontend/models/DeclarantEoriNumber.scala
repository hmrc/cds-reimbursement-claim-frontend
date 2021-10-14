package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import play.api.libs.json.{Json, OFormat}

final case class DeclarantEoriNumber(value: Eori)

object DeclarantEoriNumber {
  implicit val format: OFormat[DeclarantEoriNumber] = Json.format[DeclarantEoriNumber]
}
