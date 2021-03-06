/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Arbitrary
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarationDetailsController.EntryDeclarationDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DateOfImport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PhoneNumberGen._

object EnterDeclarationDetailsGen {

  implicit val arbitraryDateOfImport: Typeclass[DateOfImport] = Arbitrary(
    arbitraryLocalDate.arbitrary.map(DateOfImport(_))
  )

  implicit val arbitraryEntryDeclarationDetails: Typeclass[EntryDeclarationDetails] = Arbitrary(
    for {
      dateOfImport          <- arbitraryDateOfImport.arbitrary
      placeOfImport         <- arbitraryString.arbitrary.map(_.take(70))
      importerName          <- arbitraryString.arbitrary.map(_.take(70))
      importerEmailAddress  <- arbitraryEmail.arbitrary
      importerPhoneNumber   <- arbitraryPhoneNumber.arbitrary
      declarantName         <- arbitraryString.arbitrary.map(_.take(70))
      declarantEmailAddress <- arbitraryEmail.arbitrary
      declarantPhoneNumber  <- arbitraryPhoneNumber.arbitrary
    } yield EntryDeclarationDetails(
      dateOfImport,
      placeOfImport,
      importerName,
      importerEmailAddress,
      importerPhoneNumber,
      declarantName,
      declarantEmailAddress,
      declarantPhoneNumber
    )
  )
}
