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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswerSpec._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReimbursementGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.TaxCodeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

import scala.collection.immutable.SortedMap

class SelectedDutyTaxCodesReimbursementAnswerSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {
  import SelectedDutyTaxCodesReimbursementAnswer._

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 3)

  "Selected reimbursement duties and tax codes" should {
    "be ordered by positioning" in {

      val answer = SelectedDutyTaxCodesReimbursementAnswer(
        value = SortedMap(
          Beer     -> SortedMap(
            NI444 -> AmountPaidWithCorrect.unclaimed,
            NI440 -> AmountPaidWithCorrect.unclaimed
          ),
          EuDuty   -> SortedMap(
            A90 -> AmountPaidWithCorrect.unclaimed,
            A80 -> AmountPaidWithCorrect.unclaimed
          ),
          MadeWine -> SortedMap(
            NI423 -> AmountPaidWithCorrect.unclaimed,
            NI422 -> AmountPaidWithCorrect.unclaimed
          ),
          UkDuty   -> SortedMap(
            B00 -> AmountPaidWithCorrect.unclaimed,
            A00 -> AmountPaidWithCorrect.unclaimed
          )
        )
      )

      answer should be(
        SelectedDutyTaxCodesReimbursementAnswer(
          value = SortedMap(
            UkDuty   -> SortedMap(
              A00 -> AmountPaidWithCorrect.unclaimed,
              B00 -> AmountPaidWithCorrect.unclaimed
            ),
            EuDuty   -> SortedMap(
              A80 -> AmountPaidWithCorrect.unclaimed,
              A90 -> AmountPaidWithCorrect.unclaimed
            ),
            Beer     -> SortedMap(
              NI440 -> AmountPaidWithCorrect.unclaimed,
              NI444 -> AmountPaidWithCorrect.unclaimed
            ),
            MadeWine -> SortedMap(
              NI422 -> AmountPaidWithCorrect.unclaimed,
              NI423 -> AmountPaidWithCorrect.unclaimed
            )
          )
        )
      )
    }
  }

  "The answer" should {
    "provide selected tax codes for given duty" in forAll { (duty: DutyType, taxCodes: List[TaxCode]) =>
      SelectedDutyTaxCodesReimbursementAnswer(
        SortedMap(
          duty -> SortedMap(
            taxCodes.map(_ -> AmountPaidWithCorrect.unclaimed): _*
          )
        )
      ).getTaxCodes(duty) should contain allElementsOf taxCodes
    }

    "find next available duty" in forAll { previousDuty: DutyType =>
      val duties = DutyTypes.all.toList
      val answer = SelectedDutyTaxCodesReimbursementAnswer buildFrom duties

      val nextDutyIndex = duties.indexOf(previousDuty) + 1
      val maybeNextDuty = duties.lift(nextDutyIndex)

      answer findNextSelectedDutyAfter previousDuty should be(maybeNextDuty)
    }

    "combine same tax code reimbursements" in forAll(genTaxCode, genDutiesWithReimbursements) { (taxCode, claims) =>
      SelectedDutyTaxCodesReimbursementAnswer(
        SortedMap(
          claims.map(claim => claim._1 -> SortedMap(taxCode -> claim._2)): _*
        )
      ).combine should be(
        Option(claims)
          .filter(_.nonEmpty)
          .map(dutiesWithReimbursements =>
            Map(
              taxCode -> dutiesWithReimbursements
                .map(_._2)
                .foldLeft(AmountPaidWithCorrect.unclaimed)((resulting, current) =>
                  AmountPaidWithCorrect(
                    paidAmount = resulting.paidAmount + current.paidAmount,
                    correctAmount = resulting.correctAmount + current.correctAmount
                  )
                )
            )
          )
      )
    }

    "find unclaimed reimbursement" in forAll(genDuty, genTaxCodesWithUnclaimedReimbursement) { (duty, claims) =>
      SelectedDutyTaxCodesReimbursementAnswer(
        SortedMap(duty -> SortedMap(claims.map(claim => claim._1 -> claim._2): _*))
      ).findUnclaimedReimbursement.isDefined should be(true)
    }
  }
}

object SelectedDutyTaxCodesReimbursementAnswerSpec {

  lazy val genDutiesWithReimbursements: Gen[List[(DutyType, AmountPaidWithCorrect)]] = for {
    dutyTypes      <- Gen.listOf(genDuty).map(_.distinct)
    reimbursements <- Gen.listOfN(dutyTypes.size, genReimbursement)
  } yield dutyTypes zip reimbursements

  lazy val genTaxCodesWithUnclaimedReimbursement: Gen[List[(TaxCode, AmountPaidWithCorrect)]] = for {
    taxCodes       <- Gen.nonEmptyListOf(genTaxCode).map(_.distinct)
    position       <- Gen.choose(0, taxCodes.length - 1)
    reimbursements <- Gen.listOfN(taxCodes.length - 1, genReimbursement)
    (front, back)   = reimbursements splitAt position
    withUnclaimed   = front ++ List(AmountPaidWithCorrect.unclaimed) ++ back
  } yield taxCodes zip withUnclaimed
}
