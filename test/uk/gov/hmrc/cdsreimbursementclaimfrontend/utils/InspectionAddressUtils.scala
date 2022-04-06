package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{InspectionAddress, InspectionAddressType}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails

trait InspectionAddressUtils {
  def inspectionAddressFromContactDetails(
    contactDetails: ContactDetails,
    inspectionAddressType: InspectionAddressType
  ): InspectionAddress =
    InspectionAddress(
      addressLine1 = contactDetails.addressLine1,
      addressLine2 = contactDetails.addressLine2,
      addressLine3 = contactDetails.addressLine3,
      city = contactDetails.addressLine4,
      countryCode = contactDetails.countryCode,
      postalCode = contactDetails.postalCode,
      addressType = inspectionAddressType
    )
}
