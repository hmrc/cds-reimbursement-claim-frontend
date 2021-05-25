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

import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarationDetailsController.EntryDeclarationDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DateOfImport
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.PhoneNumberGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber

import java.time.LocalDate

object EnterDeclarationDetailsGen extends GenUtils {

  implicit val dateOfImportGen: Gen[DateOfImport] =
    gen[LocalDate].map(DateOfImport(_))

  implicit val dateOfImportArb: Arbitrary[DateOfImport] =
    Arbitrary(dateOfImportGen)

  implicit val phoneNumberArb: Arbitrary[PhoneNumber] =
    Arbitrary(phoneNumberGen)

  implicit val emailArb: Arbitrary[Email] =
    Arbitrary(emailGen)

  implicit val entryDeclarationDetailsGen: Gen[EntryDeclarationDetails] = gen[EntryDeclarationDetails]
}
