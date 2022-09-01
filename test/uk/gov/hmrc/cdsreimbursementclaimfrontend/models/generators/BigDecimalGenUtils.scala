/*
 * Copyright 2022 HM Revenue & Customs
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

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

trait BigDecimalGen {

  implicit final val bigDecimalChoose = new Gen.Choose[BigDecimal] {
    override def choose(min: BigDecimal, max: BigDecimal): Gen[BigDecimal] =
      Gen
        .choose(1, 10000)
        .map(i => (min + (i * ((max - min) / 10000))))
        .map(bd => BigDecimal(bd.*(100).toInt)./(100))
  }

  final lazy val amountNumberGen: Gen[BigDecimal] =
    amountNumberInRangeGen(BigDecimal("1.00"), BigDecimal("1000.00"))

  final def amountNumberInRangeGen(minIncl: BigDecimal, maxIncl: BigDecimal): Gen[BigDecimal] =
    Gen.choose[BigDecimal](minIncl, maxIncl)

  implicit lazy val amountNumberArbitrary: Arbitrary[BigDecimal] =
    Arbitrary(amountNumberGen)
}

object BigDecimalGen extends BigDecimalGen
