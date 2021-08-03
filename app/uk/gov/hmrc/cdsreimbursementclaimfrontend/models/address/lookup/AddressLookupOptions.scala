package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions._

final case class AddressLookupOptions(
  continueUrl: String,
  signOutHref: String,
  accessibilityFooterUrl: String,
  selectPageConfig: SelectPageConfig,
  confirmPageConfig: ConfirmPageConfig,
  timeoutConfig: TimeoutConfig,
  phaseFeedbackLink: String = "private-beta",
  deskProServiceName: String = "cds-reimbursement-claim",
  showPhaseBanner: Boolean = true,
  ukMode: Boolean = true
)

object AddressLookupOptions {

  final case class SelectPageConfig(proposalListLimit: Int)

  final case class ConfirmPageConfig(
    showChangeLink: Boolean,
    showSearchAgainLink: Boolean,
    showConfirmChangeText: Boolean
  )

  final case class TimeoutConfig(
    timeoutAmount: Long,
    timeoutUrl: String,
    timeoutKeepAliveUrl: String
  )

  implicit val format: OFormat[AddressLookupLabels] = Json.format[AddressLookupLabels]
}
