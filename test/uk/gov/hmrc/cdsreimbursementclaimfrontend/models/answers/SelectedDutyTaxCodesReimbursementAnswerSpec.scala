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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DutyType, DutyTypes, Reimbursement, TaxCode}

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
            NI444 -> Reimbursement.unclaimed,
            NI440 -> Reimbursement.unclaimed
          ),
          EuDuty   -> SortedMap(
            A90 -> Reimbursement.unclaimed,
            A80 -> Reimbursement.unclaimed
          ),
          MadeWine -> SortedMap(
            NI423 -> Reimbursement.unclaimed,
            NI422 -> Reimbursement.unclaimed
          ),
          UkDuty   -> SortedMap(
            B00 -> Reimbursement.unclaimed,
            A00 -> Reimbursement.unclaimed
          )
        )
      )

      answer should be(
        SelectedDutyTaxCodesReimbursementAnswer(
          value = SortedMap(
            UkDuty   -> SortedMap(
              A00 -> Reimbursement.unclaimed,
              B00 -> Reimbursement.unclaimed
            ),
            EuDuty   -> SortedMap(
              A80 -> Reimbursement.unclaimed,
              A90 -> Reimbursement.unclaimed
            ),
            Beer     -> SortedMap(
              NI440 -> Reimbursement.unclaimed,
              NI444 -> Reimbursement.unclaimed
            ),
            MadeWine -> SortedMap(
              NI422 -> Reimbursement.unclaimed,
              NI423 -> Reimbursement.unclaimed
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
            taxCodes.map(_ -> Reimbursement.unclaimed): _*
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
                .foldLeft(Reimbursement.unclaimed)((resulting, current) =>
                  Reimbursement(
                    paidAmount = resulting.paidAmount + current.paidAmount,
                    shouldOfPaid = resulting.shouldOfPaid + current.shouldOfPaid
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

    "calculate total & subtotal" in {}
  }
}

object SelectedDutyTaxCodesReimbursementAnswerSpec {

  lazy val genDutiesWithReimbursements: Gen[List[(DutyType, Reimbursement)]] = for {
    dutyTypes      <- Gen.listOf(genDuty).map(_.distinct)
    reimbursements <- Gen.listOfN(dutyTypes.size, genReimbursement)
  } yield dutyTypes zip reimbursements

  lazy val genTaxCodesWithUnclaimedReimbursement: Gen[List[(TaxCode, Reimbursement)]] = for {
    taxCodes       <- Gen.nonEmptyListOf(genTaxCode).map(_.distinct)
    position       <- Gen.choose(0, taxCodes.length - 1)
    reimbursements <- Gen.listOfN(taxCodes.length - 1, genReimbursement)
    (front, back)   = reimbursements splitAt position
    withUnclaimed   = front ++ List(Reimbursement.unclaimed) ++ back
  } yield taxCodes zip withUnclaimed
}
