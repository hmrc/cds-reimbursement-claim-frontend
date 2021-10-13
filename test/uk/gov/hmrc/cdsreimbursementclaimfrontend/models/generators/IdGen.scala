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

import org.scalacheck.magnolia._
import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{AssociatedMrnIndex, EntryNumber, GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Eori, MovementReferenceNumber}

object IdGen {

  implicit val arbitraryGGCredIdGen: Typeclass[GGCredId] = gen[GGCredId]

  implicit val arbitraryUploadReference: Typeclass[UploadReference] = gen[UploadReference]

  val genAssociatedMrnIndex = Gen
    .chooseNum(0, 100)
    .map(AssociatedMrnIndex.fromListIndex)

  implicit val arbitraryAssociatedMrnIndex: Typeclass[AssociatedMrnIndex] =
    Arbitrary(genAssociatedMrnIndex)

  val genEori: Gen[Eori] =
    for {
      c <- Gen.listOfN(2, Gen.alphaUpperChar)
      n <- Gen.listOfN(12, Gen.numChar)
      s <- Gen.const((c ++ n).mkString(""))
    } yield Eori(s)

  implicit val arbitraryEori: Typeclass[Eori] = Arbitrary(genEori)

  val genEntryNumber: Gen[EntryNumber] = for {
    prefix <- Gen.listOfN(9, Gen.numChar)
    letter <- Gen.listOfN(1, Gen.alphaUpperChar)
    suffix <- Gen.listOfN(8, Gen.numChar)
  } yield EntryNumber((prefix ++ letter ++ suffix).mkString)

  val genMRN: Gen[MRN] = for {
    d1      <- Gen.listOfN(2, Gen.numChar)
    letter2 <- Gen.listOfN(2, Gen.alphaUpperChar)
    word    <- Gen.listOfN(13, Gen.numChar)
    d2      <- Gen.listOfN(1, Gen.numChar)
  } yield MRN((d1 ++ letter2 ++ word ++ d2).mkString)

  implicit val arbitraryMrn: Typeclass[MRN] = Arbitrary(genMRN)

  val genMovementReferenceNumber: Gen[MovementReferenceNumber] =
    genMRN.map(MovementReferenceNumber(_))

  implicit val arbitraryMovementReferenceNumber: Typeclass[MovementReferenceNumber] =
    Arbitrary(genMovementReferenceNumber)
}
