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
import org.scalacheck.Gen
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber

object PhoneNumberGen {

  lazy val genUkPhoneNumber: Gen[PhoneNumber] =
    Gen.listOfN(10, Gen.numChar).map(numbers => PhoneNumber(numbers.foldLeft("0")((s, ch) => s"$s$ch")))

  implicit lazy val arbitraryPhoneNumber: Typeclass[PhoneNumber] = Arbitrary(genUkPhoneNumber)
}
