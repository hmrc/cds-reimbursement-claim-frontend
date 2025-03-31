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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Name
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

object IdGen {

  lazy val genCaseNumber: Gen[String] =
    Gen.listOfN(25, Gen.numChar).map(_.mkString(""))

  lazy val genAssociatedMrnIndex: Gen[AssociatedMrnIndex] = Gen
    .chooseNum(0, 100)
    .map(AssociatedMrnIndex.fromListIndex)

  implicit lazy val arbitraryAssociatedMrnIndex: Arbitrary[AssociatedMrnIndex] =
    Arbitrary(genAssociatedMrnIndex)

  lazy val genDan: Gen[Dan] =
    for
      n <- Gen.listOfN(7, Gen.numChar)
      s <- Gen.const(n.mkString)
    yield Dan(s)

  lazy val genEori: Gen[Eori] =
    for
      n <- Gen.listOfN(12, Gen.numChar)
      s <- Gen.const(s"GB${n.mkString}")
    yield Eori(s)

  val eoriPrefixes = Seq(
    "BG",
    "CZ",
    "DK",
    "DE",
    "EE",
    "IE",
    "EL",
    "ES",
    "FR",
    "HR",
    "IT",
    "CY",
    "LV",
    "LT",
    "LU",
    "HU",
    "MT",
    "NL",
    "AT",
    "PL",
    "PT",
    "RO",
    "SI",
    "SK",
    "FI",
    "SE",
    "UK"
  )

  lazy val genNewEoriFormat: Gen[Eori] =
    for
      prefix <- Gen.oneOf(eoriPrefixes)
      suffix <- genStringWithMaxSizeOfN(15)
    yield Eori(prefix + suffix)

  lazy val genXiEori: Gen[Eori] =
    for
      n <- Gen.listOfN(12, Gen.numChar)
      s <- Gen.const(s"XI${n.mkString}")
    yield Eori(s)

  implicit lazy val arbitraryEori: Arbitrary[Eori] = Arbitrary(genEori)

  lazy val genMRN: Gen[MRN] = for
    d1      <- Gen.listOfN(2, Gen.numChar)
    letter2 <- Gen.listOfN(2, Gen.alphaUpperChar)
    word    <- Gen.listOfN(13, Gen.numChar)
    d2      <- Gen.listOfN(1, Gen.numChar)
  yield MRN((d1 ++ letter2 ++ word ++ d2).mkString)

  lazy val genName: Gen[Name] = for
    name     <- genStringWithMaxSizeOfN(20)
    lastName <- genStringWithMaxSizeOfN(20)
  yield Name(Some(name), Some(lastName))

  implicit lazy val arbitraryName: Arbitrary[Name] = Arbitrary(genName)

  implicit lazy val arbitraryMrn: Arbitrary[MRN] = Arbitrary(genMRN)

}
