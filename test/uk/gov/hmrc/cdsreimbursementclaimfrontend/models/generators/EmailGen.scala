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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email

object EmailGen {

  implicit val arbitraryEmail: Typeclass[Email] = Arbitrary(
    for {
      name   <- genNonEmptyStr(Gen.alphaLowerChar, max = 15)
      at      = "@"
      domain <- genNonEmptyStr(Gen.alphaLowerChar, max = 10)
      dotCom  = ".com"
    } yield Email(Seq(name, at, domain, dotCom).mkString)
  )

  private def genNonEmptyStr(gen: Gen[Char], max: Int): Gen[String] =
    Gen.chooseNum(1, max) flatMap (Gen.listOfN(_, gen).map(_.mkString))
}
