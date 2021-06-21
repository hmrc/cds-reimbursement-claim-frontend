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
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Eori, MovementReferenceNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference

object IdGen {

  implicit val arbitraryGGCredIdGen: Typeclass[GGCredId] = gen[GGCredId]

  implicit val arbitraryUploadReference: Typeclass[UploadReference] = gen[UploadReference]

  implicit val arbitraryEori: Typeclass[Eori] = Arbitrary(for {
    c <- Gen.listOfN(2, Gen.alphaUpperChar)
    n <- Gen.listOfN(12, Gen.numChar)
    s <- Gen.const((c ++ n).mkString(""))
  } yield Eori(s))

  def genMRN: Gen[MRN] = for {
    d1      <- Gen.listOfN(2, Gen.numChar)
    letter2 <- Gen.listOfN(2, Gen.alphaUpperChar)
    word    <- Gen.listOfN(13, Gen.numChar)
    d2      <- Gen.listOfN(1, Gen.numChar)
  } yield MRN((d1 ++ letter2 ++ word ++ d2).mkString)

  implicit val arbitraryMrn: Typeclass[MRN] = Arbitrary(genMRN)

  def sampleMrnAnswer(mrn: MRN = sample[MRN]): Option[MovementReferenceNumber] =
    Some(MovementReferenceNumber(Right(mrn)))

  def genEntryNumber: Gen[EntryNumber] = for {
    prefix <- Gen.listOfN(9, Gen.numChar)
    letter <- Gen.listOfN(1, Gen.alphaUpperChar)
    suffix <- Gen.listOfN(8, Gen.numChar)
  } yield EntryNumber((prefix ++ letter ++ suffix).mkString)

  implicit val entryNumberGen: Typeclass[EntryNumber] = Arbitrary(genEntryNumber)

  def sampleEntryNumberAnswer(entryNumber: EntryNumber = sample[EntryNumber]): Option[MovementReferenceNumber] =
    Some(MovementReferenceNumber(Left(entryNumber)))

}
