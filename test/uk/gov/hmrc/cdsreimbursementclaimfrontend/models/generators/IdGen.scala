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
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference

object IdGen {

  implicit val arbitraryGGCredIdGen: Typeclass[GGCredId] = gen[GGCredId]

  implicit val uploadReferenceGen: Gen[UploadReference] = gen[UploadReference]

  implicit val arbitraryEori: Typeclass[Eori] = Arbitrary(for {
    c <- Gen.listOfN(2, Gen.alphaUpperChar)
    n <- Gen.listOfN(12, Gen.numChar)
    s <- Gen.const((c ++ n).mkString(""))
  } yield Eori(s))

  implicit val arbitraryMrn: Typeclass[MRN] = Arbitrary(
    genReference.map(ref => MRN(ref.mkString("")))
  )

  implicit val entryNumberGen: Typeclass[EntryNumber] = Arbitrary(
    genReference.map(ref => EntryNumber(ref.mkString("")))
  )

  private def genReference = for {
    d1      <- Gen.listOfN(2, Gen.numChar)
    letter2 <- Gen.listOfN(2, Gen.alphaUpperChar)
    word    <- Gen.listOfN(13, Gen.numChar)
    d2      <- Gen.listOfN(1, Gen.numChar)
  } yield d1 ++ letter2 ++ word ++ d2
}
