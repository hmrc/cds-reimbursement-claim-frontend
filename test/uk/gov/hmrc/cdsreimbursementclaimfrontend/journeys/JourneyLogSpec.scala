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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OrderedMap
import scala.collection.immutable.SortedMap
import java.time.LocalDate

class JourneyLogSpec extends AnyWordSpec with Matchers with JourneyTestData {

  "JourneyOutputLogger" should {

    "log OverpaymentsSingleJourney.Output" in {
      val output: OverpaymentsSingleJourney.Output =
        OverpaymentsSingleJourney.Output(
          movementReferenceNumber = exampleMrn,
          duplicateMovementReferenceNumber = Some(anotherExampleMrn),
          claimantType = ClaimantType.Consignee,
          claimantInformation = exampleClaimantInformation,
          basisOfClaim = BasisOfOverpaymentClaim.OutwardProcessingRelief,
          whetherNorthernIreland = true,
          additionalDetails = "foo bar",
          reimbursementClaims = Map(TaxCode.A00 -> BigDecimal("1234.56"), TaxCode.A30 -> BigDecimal("12.34")),
          reimbursementMethod = ReimbursementMethod.CurrentMonthAdjustment,
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences
        )

      val log = JourneyLog.apply(output)

      log.journeyType                        shouldBe "overpayments"
      log.journeyVariant                     shouldBe "single"
      log.numberOfMultipleMRNs               shouldBe None
      log.claimantType                       shouldBe "Consignee"
      log.basisOfClaim                       shouldBe Some("OutwardProcessingRelief")
      log.basisOfClaimSpecialCircumstances   shouldBe None
      log.methodOfDisposal                   shouldBe None
      log.whetherNorthernIreland             shouldBe Some(true)
      log.reasonForSecurity                  shouldBe None
      log.temporaryAdmissionMethodOfDisposal shouldBe None
      log.reimbursementMethod                shouldBe "CurrentMonthAdjustment"
      log.claimedAmountThreshold             shouldBe "4"
      log.claimedDuties                      shouldBe Seq("A00", "A30")
      log.numberOfClaimedDuties              shouldBe 2
      log.numberOfEvidenceFilesAttached      shouldBe 2
      log.documentTypesAttached              shouldBe Seq("CommercialInvoice", "Other")
      log.fileTypesAttached                  shouldBe Seq("application/pdf", "image/jpeg")
      log.fileSizes                          shouldBe Seq(567L, 1234L)
      log.scheduleFileType                   shouldBe None
      log.scheduleFileSize                   shouldBe None
    }

    "log OverpaymentsMultipleJourney.Output" in {
      val output: OverpaymentsMultipleJourney.Output =
        OverpaymentsMultipleJourney.Output(
          movementReferenceNumbers = Seq(exampleMrn, anotherExampleMrn),
          claimantType = ClaimantType.Declarant,
          claimantInformation = exampleClaimantInformation,
          basisOfClaim = BasisOfOverpaymentClaim.IncorrectAdditionalInformationCode,
          whetherNorthernIreland = false,
          additionalDetails = "foo bar",
          reimbursementClaims = OrderedMap.from(
            Seq(
              exampleMrn        -> Map(TaxCode.A00 -> BigDecimal("1234.56")),
              anotherExampleMrn -> Map(TaxCode.A30 -> BigDecimal("12.34"), TaxCode.A90 -> BigDecimal("123456"))
            )
          ),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences
        )

      val log = JourneyLog.apply(output)

      log.journeyType                        shouldBe "overpayments"
      log.journeyVariant                     shouldBe "multiple"
      log.numberOfMultipleMRNs               shouldBe Some(2)
      log.claimantType                       shouldBe "Declarant"
      log.basisOfClaim                       shouldBe Some("IncorrectAdditionalInformationCode")
      log.basisOfClaimSpecialCircumstances   shouldBe None
      log.methodOfDisposal                   shouldBe None
      log.whetherNorthernIreland             shouldBe Some(false)
      log.reasonForSecurity                  shouldBe None
      log.temporaryAdmissionMethodOfDisposal shouldBe None
      log.reimbursementMethod                shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold             shouldBe "6"
      log.claimedDuties                      shouldBe Seq("A00", "A30", "A90")
      log.numberOfClaimedDuties              shouldBe 3
      log.numberOfEvidenceFilesAttached      shouldBe 2
      log.documentTypesAttached              shouldBe Seq("CommercialInvoice", "Other")
      log.fileTypesAttached                  shouldBe Seq("application/pdf", "image/jpeg")
      log.fileSizes                          shouldBe Seq(567L, 1234L)
      log.scheduleFileType                   shouldBe None
      log.scheduleFileSize                   shouldBe None
    }

    "log OverpaymentsScheduledJourney.Output" in {
      val output: OverpaymentsScheduledJourney.Output =
        OverpaymentsScheduledJourney.Output(
          movementReferenceNumber = exampleMrn,
          scheduledDocument = exampleScheduledDocument,
          claimantType = ClaimantType.User,
          claimantInformation = exampleClaimantInformation,
          basisOfClaim = BasisOfOverpaymentClaim.PersonalEffects,
          whetherNorthernIreland = false,
          additionalDetails = "foo bar",
          reimbursementClaims = SortedMap.from(
            Seq(
              DutyType.UkDuty -> SortedMap.from(
                Seq(
                  TaxCode.A30 -> AmountPaidWithCorrect(BigDecimal("200000"), BigDecimal("1234.56"))
                )
              ),
              DutyType.EuDuty -> SortedMap.from(
                Seq(
                  TaxCode.A00 -> AmountPaidWithCorrect(BigDecimal("2000"), BigDecimal("12.34"))
                )
              )
            )
          ),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences
        )

      val log = JourneyLog.apply(output)

      println(log)

      log.journeyType                        shouldBe "overpayments"
      log.journeyVariant                     shouldBe "scheduled"
      log.numberOfMultipleMRNs               shouldBe None
      log.claimantType                       shouldBe "User"
      log.basisOfClaim                       shouldBe Some("PersonalEffects")
      log.basisOfClaimSpecialCircumstances   shouldBe None
      log.methodOfDisposal                   shouldBe None
      log.whetherNorthernIreland             shouldBe Some(false)
      log.reasonForSecurity                  shouldBe None
      log.temporaryAdmissionMethodOfDisposal shouldBe None
      log.reimbursementMethod                shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold             shouldBe "6"
      log.claimedDuties                      shouldBe Seq("A00", "A30")
      log.numberOfClaimedDuties              shouldBe 2
      log.numberOfEvidenceFilesAttached      shouldBe 2
      log.documentTypesAttached              shouldBe Seq("CommercialInvoice", "Other")
      log.fileTypesAttached                  shouldBe Seq("application/pdf", "image/jpeg")
      log.fileSizes                          shouldBe Seq(567L, 1234L)
      log.scheduleFileType                   shouldBe Some("image/png")
      log.scheduleFileSize                   shouldBe Some(9876L)
    }

    "log RejectedGoodsSingleJourney.Output" in {
      val output: RejectedGoodsSingleJourney.Output =
        RejectedGoodsSingleJourney.Output(
          movementReferenceNumber = exampleMrn,
          claimantType = ClaimantType.Consignee,
          claimantInformation = exampleClaimantInformation,
          basisOfClaim = BasisOfRejectedGoodsClaim.DamagedBeforeClearance,
          basisOfClaimSpecialCircumstances = Some("BasisOfClaimSpecialCircumstances"),
          methodOfDisposal = MethodOfDisposal.Destruction,
          detailsOfRejectedGoods = "foo bar",
          inspectionDate = InspectionDate(LocalDate.now()),
          inspectionAddress = exampleInspectionAddress,
          reimbursementClaims = Map(
            TaxCode.A00 -> BigDecimal("1234.56"),
            TaxCode.A90 -> BigDecimal("12.34"),
            TaxCode.A20 -> BigDecimal("1234789")
          ),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences
        )

      val log = JourneyLog.apply(output)

      log.journeyType                        shouldBe "rejectedgoods"
      log.journeyVariant                     shouldBe "single"
      log.numberOfMultipleMRNs               shouldBe None
      log.claimantType                       shouldBe "Consignee"
      log.basisOfClaim                       shouldBe Some("DamagedBeforeClearance")
      log.basisOfClaimSpecialCircumstances   shouldBe Some("BasisOfClaimSpecialCircumstances")
      log.methodOfDisposal                   shouldBe Some("Destruction")
      log.whetherNorthernIreland             shouldBe None
      log.reasonForSecurity                  shouldBe None
      log.temporaryAdmissionMethodOfDisposal shouldBe None
      log.reimbursementMethod                shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold             shouldBe "7"
      log.claimedDuties                      shouldBe Seq("A00", "A20", "A90")
      log.numberOfClaimedDuties              shouldBe 3
      log.numberOfEvidenceFilesAttached      shouldBe 2
      log.documentTypesAttached              shouldBe Seq("CommercialInvoice", "Other")
      log.fileTypesAttached                  shouldBe Seq("application/pdf", "image/jpeg")
      log.fileSizes                          shouldBe Seq(567L, 1234L)
      log.scheduleFileType                   shouldBe None
      log.scheduleFileSize                   shouldBe None
    }

    "log RejectedGoodsMultipleJourney.Output" in {
      val output: RejectedGoodsMultipleJourney.Output =
        RejectedGoodsMultipleJourney.Output(
          movementReferenceNumbers = Seq(exampleMrn, anotherExampleMrn),
          claimantType = ClaimantType.Consignee,
          claimantInformation = exampleClaimantInformation,
          basisOfClaim = BasisOfRejectedGoodsClaim.DamagedBeforeClearance,
          basisOfClaimSpecialCircumstances = Some("BasisOfClaimSpecialCircumstances"),
          methodOfDisposal = MethodOfDisposal.Destruction,
          detailsOfRejectedGoods = "foo bar",
          inspectionDate = InspectionDate(LocalDate.now()),
          inspectionAddress = exampleInspectionAddress,
          reimbursementClaims = OrderedMap.from(
            Seq(
              exampleMrn        -> Map(TaxCode.A00 -> BigDecimal("1234.56"), TaxCode.A90 -> BigDecimal("123456")),
              anotherExampleMrn -> Map(TaxCode.A30 -> BigDecimal("12.34"))
            )
          ),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences
        )

      val log = JourneyLog.apply(output)

      log.journeyType                        shouldBe "rejectedgoods"
      log.journeyVariant                     shouldBe "multiple"
      log.numberOfMultipleMRNs               shouldBe Some(2)
      log.claimantType                       shouldBe "Consignee"
      log.basisOfClaim                       shouldBe Some("DamagedBeforeClearance")
      log.basisOfClaimSpecialCircumstances   shouldBe Some("BasisOfClaimSpecialCircumstances")
      log.methodOfDisposal                   shouldBe Some("Destruction")
      log.whetherNorthernIreland             shouldBe None
      log.reasonForSecurity                  shouldBe None
      log.temporaryAdmissionMethodOfDisposal shouldBe None
      log.reimbursementMethod                shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold             shouldBe "6"
      log.claimedDuties                      shouldBe Seq("A00", "A30", "A90")
      log.numberOfClaimedDuties              shouldBe 3
      log.numberOfEvidenceFilesAttached      shouldBe 2
      log.documentTypesAttached              shouldBe Seq("CommercialInvoice", "Other")
      log.fileTypesAttached                  shouldBe Seq("application/pdf", "image/jpeg")
      log.fileSizes                          shouldBe Seq(567L, 1234L)
      log.scheduleFileType                   shouldBe None
      log.scheduleFileSize                   shouldBe None
    }

    "log RejectedGoodsScheduledJourney.Output" in {
      val output: RejectedGoodsScheduledJourney.Output =
        RejectedGoodsScheduledJourney.Output(
          movementReferenceNumber = exampleMrn,
          scheduledDocument = exampleScheduledDocument,
          claimantType = ClaimantType.User,
          claimantInformation = exampleClaimantInformation,
          basisOfClaim = BasisOfRejectedGoodsClaim.DamagedBeforeClearance,
          basisOfClaimSpecialCircumstances = Some("BasisOfClaimSpecialCircumstances"),
          methodOfDisposal = MethodOfDisposal.Destruction,
          detailsOfRejectedGoods = "foo bar",
          inspectionDate = InspectionDate(LocalDate.now()),
          inspectionAddress = exampleInspectionAddress,
          reimbursementClaims = SortedMap.from(
            Seq(
              DutyType.UkDuty -> SortedMap.from(
                Seq(
                  TaxCode.A90 -> AmountPaidWithRefund(BigDecimal("200000"), BigDecimal("84.56"))
                )
              ),
              DutyType.EuDuty -> SortedMap.from(
                Seq(
                  TaxCode.A80 -> AmountPaidWithRefund(BigDecimal("2000"), BigDecimal("42.34"))
                )
              )
            )
          ),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences
        )

      val log = JourneyLog.apply(output)

      log.journeyType                        shouldBe "rejectedgoods"
      log.journeyVariant                     shouldBe "scheduled"
      log.numberOfMultipleMRNs               shouldBe None
      log.claimantType                       shouldBe "User"
      log.basisOfClaim                       shouldBe Some("DamagedBeforeClearance")
      log.basisOfClaimSpecialCircumstances   shouldBe Some("BasisOfClaimSpecialCircumstances")
      log.methodOfDisposal                   shouldBe Some("Destruction")
      log.whetherNorthernIreland             shouldBe None
      log.reasonForSecurity                  shouldBe None
      log.temporaryAdmissionMethodOfDisposal shouldBe None
      log.reimbursementMethod                shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold             shouldBe "3"
      log.claimedDuties                      shouldBe Seq("A80", "A90")
      log.numberOfClaimedDuties              shouldBe 2
      log.numberOfEvidenceFilesAttached      shouldBe 2
      log.documentTypesAttached              shouldBe Seq("CommercialInvoice", "Other")
      log.fileTypesAttached                  shouldBe Seq("application/pdf", "image/jpeg")
      log.fileSizes                          shouldBe Seq(567L, 1234L)
      log.scheduleFileType                   shouldBe Some("image/png")
      log.scheduleFileSize                   shouldBe Some(9876L)
    }

    "log SecuritiesJourney.Output" in {
      val output: SecuritiesJourney.Output =
        SecuritiesJourney.Output(
          movementReferenceNumber = exampleMrn,
          claimantType = ClaimantType.Consignee,
          claimantInformation = exampleClaimantInformation,
          reasonForSecurity = ReasonForSecurity.MissingLicenseQuota,
          securitiesReclaims = SortedMap
            .from(
              Seq(
                "ABC" -> SortedMap.from[TaxCode, BigDecimal](Seq(TaxCode.A45 -> BigDecimal("1234.56"))),
                "def" -> SortedMap.from[TaxCode, BigDecimal](
                  Seq(TaxCode.A30 -> BigDecimal("12.34"), TaxCode.A90 -> BigDecimal("12345"))
                )
              )
            ),
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences,
          temporaryAdmissionMethodOfDisposal = Some(TemporaryAdmissionMethodOfDisposal.DeclaredToEndUse),
          exportMovementReferenceNumber = Some(anotherExampleMrn)
        )

      val log = JourneyLog.apply(output)

      log.journeyType                        shouldBe "securities"
      log.journeyVariant                     shouldBe "single"
      log.numberOfMultipleMRNs               shouldBe None
      log.claimantType                       shouldBe "Consignee"
      log.basisOfClaim                       shouldBe None
      log.basisOfClaimSpecialCircumstances   shouldBe None
      log.methodOfDisposal                   shouldBe None
      log.whetherNorthernIreland             shouldBe None
      log.reasonForSecurity                  shouldBe Some("MissingLicenseQuota")
      log.temporaryAdmissionMethodOfDisposal shouldBe Some("DeclaredToEndUse")
      log.reimbursementMethod                shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold             shouldBe "5"
      log.claimedDuties                      shouldBe Seq("A30", "A45", "A90")
      log.numberOfClaimedDuties              shouldBe 3
      log.numberOfEvidenceFilesAttached      shouldBe 2
      log.documentTypesAttached              shouldBe Seq("CommercialInvoice", "Other")
      log.fileTypesAttached                  shouldBe Seq("application/pdf", "image/jpeg")
      log.fileSizes                          shouldBe Seq(567L, 1234L)
      log.scheduleFileType                   shouldBe None
      log.scheduleFileSize                   shouldBe None
    }

  }
}
