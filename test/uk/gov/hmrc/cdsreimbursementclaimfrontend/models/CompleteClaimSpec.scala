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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{ConsigneeBankDetails, DisplayDeclaration, DisplayResponseDetail, MaskedBankDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.CompleteClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

class CompleteClaimSpec extends AnyWordSpec with Matchers {

  "BankAccountView Extraction" should {
    "Use the manually entered bank account, and not the one retrieved by ACC14 on the MRN journey" in {
      val bankAccount       = sample[BankAccountDetails]
      val completeC285Claim = sample[CompleteClaim]
        .copy(
          movementReferenceNumber = MovementReferenceNumber(Right(sample[MRN])),
          maybeBankAccountDetailsAnswer = Some(bankAccount)
        )

      val bankDetails = completeC285Claim.bankDetails.getOrElse(fail("No bank details"))
      bankDetails.accountName   shouldBe bankAccount.accountName
      bankDetails.sortCode      shouldBe bankAccount.sortCode
      bankDetails.accountNumber shouldBe bankAccount.accountNumber
    }

    "Use the maskedBankDetails from ACC14 when it was not changed manually on the MRN journey" in {
      val consigneeBankDetails  = sample[ConsigneeBankDetails]
      val bankAccount           = sample[MaskedBankDetails].copy(consigneeBankDetails = Some(consigneeBankDetails))
      val displayResponseDetail = sample[DisplayResponseDetail].copy(maskedBankDetails = Some(bankAccount))
      val completeC285Claim     = sample[CompleteClaim]
        .copy(
          movementReferenceNumber = MovementReferenceNumber(Right(sample[MRN])),
          maybeDisplayDeclaration = Some(DisplayDeclaration(displayResponseDetail)),
          maybeBankAccountDetailsAnswer = None
        )
      val bankDetails           = completeC285Claim.bankDetails.getOrElse(fail("No bank details"))
      bankDetails.accountName.value   shouldBe consigneeBankDetails.accountHolderName
      bankDetails.sortCode.value      shouldBe consigneeBankDetails.sortCode
      bankDetails.accountNumber.value shouldBe consigneeBankDetails.accountNumber
    }
  }
}
