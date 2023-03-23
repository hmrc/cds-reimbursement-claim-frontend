/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType

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
