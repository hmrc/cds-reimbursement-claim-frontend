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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{AmountPaidWithCorrect, DutyType, DutyTypes, TaxCode}

import scala.jdk.CollectionConverters.*

/** A collection of generators supporting the tests of scheduled journeys. */
trait ScheduledJourneyGenerators extends JourneyGenerators {

  final val amountPaidWithCorrectGen: Gen[AmountPaidWithCorrect] =
    for
      correctAmount <- Gen.choose(BigDecimal("0.01"), BigDecimal("1000.00"))
      random        <- Gen.choose(BigDecimal("0.01"), BigDecimal("100.00"))
      paidAmount     = random + correctAmount
    yield AmountPaidWithCorrect(paidAmount, correctAmount)

  final val amountPaidWithClaimGen: Gen[(BigDecimal, BigDecimal)] =
    for
      claimAmount <- Gen.choose(BigDecimal("0.01"), BigDecimal("1000.00"))
      random      <- Gen.choose(BigDecimal("0.01"), BigDecimal("100.00"))
      paidAmount   = random + claimAmount
    yield (paidAmount, claimAmount)

  final val dutyTypesGen: Gen[Seq[DutyType]] =
    for
      n   <- Gen.choose(2, DutyTypes.all.size - 1)
      dts <- Gen.pick(n, DutyTypes.all)
    yield dts.sorted.toSeq

  final def taxCodesGen(dutyType: DutyType): Gen[Seq[TaxCode]] =
    for
      n   <- Gen.choose(1, dutyType.taxCodes.size - 1)
      tcs <- Gen.pick(n, dutyType.taxCodes)
    yield tcs.sorted.toSeq

  final val dutyTypesWithTaxCodesGen: Gen[Seq[(DutyType, Seq[TaxCode])]] = dutyTypesGen.flatMap(dutyTypes =>
    Gen.sequence[Seq[(DutyType, Seq[TaxCode])], (DutyType, Seq[TaxCode])](
      dutyTypes.map(dutyType =>
        for
          n   <- Gen.choose(1, dutyType.taxCodes.size - 1)
          tcs <- Gen.pick(n, dutyType.taxCodes)
        yield (dutyType, tcs.sorted.toSeq)
      )
    )
  )

  type TaxCodeWithAmounts = (TaxCode, BigDecimal, BigDecimal)

  final def taxCodesWithClaimAmountsGen(dutyType: DutyType): Gen[Seq[TaxCodeWithAmounts]] =
    for
      n       <- Gen.choose(1, dutyType.taxCodes.size - 1)
      tcs     <- Gen.pick(n, dutyType.taxCodes)
      amounts <- Gen.sequence[Seq[TaxCodeWithAmounts], TaxCodeWithAmounts](
                   tcs.sorted.map(tc =>
                     amountNumberGen
                       .flatMap(paid =>
                         amountNumberInRangeGen(ZERO, paid - BigDecimal("0.01"))
                           .map(correct => (tc, paid, correct))
                       )
                   )
                 )
    yield amounts

  final val dutyTypesWithTaxCodesWithClaimAmountsGen: Gen[Seq[(DutyType, Seq[TaxCodeWithAmounts])]] =
    for
      dutyTypes <- dutyTypesGen
      result    <-
        Gen.sequence(dutyTypes.map(dutyType => taxCodesWithClaimAmountsGen(dutyType).map(tcs => dutyType -> tcs)))
    yield result.asScala.toSeq

}
