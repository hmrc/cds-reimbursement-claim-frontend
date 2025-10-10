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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyLog
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OrderedMap

import java.time.LocalDate
import scala.collection.immutable.SortedMap

class JourneyLogSpec extends AnyWordSpec with Matchers with ClaimTestData {

  case class TestClaimAnalytics(
    claimDurationSeconds: Long = 12345,
    emailAddressHasChanged: Boolean = true,
    contactNameHasChanged: Boolean = true,
    phoneNumberHasChanged: Boolean = true,
    contactAddressHasChanged: Boolean = true,
    bankAccountHasChanged: Boolean = true,
    declarantEoriMatchesConsignee: Boolean = true
  ) extends ClaimAnalytics

  "ClaimOutputLogger" should {

    "log OverpaymentsSingleClaim.Output" in {
      val output: OverpaymentsSingleClaim.Output =
        OverpaymentsSingleClaim.Output(
          movementReferenceNumber = exampleMrn,
          duplicateMovementReferenceNumber = Some(anotherExampleMrn),
          claimantType = ClaimantType.Consignee,
          payeeType = PayeeType.Declarant,
          displayPayeeType = PayeeType.Declarant,
          claimantInformation = exampleClaimantInformation,
          basisOfClaim = BasisOfOverpaymentClaim.OutwardProcessingRelief,
          additionalDetails = "foo bar",
          reimbursements = Seq(
            Reimbursement(
              TaxCode.A00,
              BigDecimal("1234.56"),
              ReimbursementMethod.CurrentMonthAdjustment,
              BigDecimal("34.56")
            ),
            Reimbursement(
              TaxCode.A30,
              BigDecimal("12.34"),
              ReimbursementMethod.CurrentMonthAdjustment,
              BigDecimal("1.34")
            )
          ),
          reimbursementMethod = ReimbursementMethod.CurrentMonthAdjustment,
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences,
          newEoriAndDan = None
        )

      val log = JourneyLog.apply(
        output,
        "123EORI",
        Some("REF-1234"),
        TestClaimAnalytics(declarantEoriMatchesConsignee = false)
      )

      log.journeyType                           shouldBe "overpayments"
      log.journeyVariant                        shouldBe "single"
      log.numberOfMultipleMRNs                  shouldBe None
      log.claimantType                          shouldBe "Consignee"
      log.consigneeIsDeclarant                  shouldBe false
      log.payeeType                             shouldBe "Declarant"
      log.basisOfClaim                          shouldBe Some("OutwardProcessingRelief")
      log.basisOfClaimSpecialCircumstances      shouldBe None
      log.methodOfDisposal                      shouldBe None
      log.reasonForSecurity                     shouldBe None
      log.temporaryAdmissionMethodsOfDisposal   shouldBe None
      log.reimbursementMethod                   shouldBe "CurrentMonthAdjustment"
      log.claimedAmountThreshold                shouldBe "4"
      log.claimedDuties                         shouldBe Seq("A00", "A30")
      log.numberOfClaimedDuties                 shouldBe 2
      log.uploads.numberOfEvidenceFilesAttached shouldBe 2
      log.uploads.documentTypesAttached         shouldBe Seq("CommercialInvoice", "Other")
      log.uploads.fileTypesAttached             shouldBe Seq("application/pdf", "image/jpeg")
      log.uploads.fileSizes                     shouldBe Seq(567L, 1234L)
      log.uploads.scheduleFileType              shouldBe None
      log.uploads.scheduleFileSize              shouldBe None
      log.caseNumber                            shouldBe Some("REF-1234")
      log.userHash                              shouldBe "931058e4"
      log.changes.emailAddress                  shouldBe true
      log.changes.contactName                   shouldBe true
      log.changes.phoneNumber                   shouldBe true
      log.changes.contactAddress                shouldBe true
      log.changes.bankAccount                   shouldBe true
      log.journeyDurationSeconds                shouldBe 12345
    }

    "log OverpaymentsMultipleClaim.Output" in {
      val output: OverpaymentsMultipleClaim.Output =
        OverpaymentsMultipleClaim.Output(
          movementReferenceNumbers = Seq(exampleMrn, anotherExampleMrn),
          claimantType = ClaimantType.Declarant,
          payeeType = PayeeType.Declarant,
          displayPayeeType = PayeeType.Declarant,
          claimantInformation = exampleClaimantInformation,
          basisOfClaim = BasisOfOverpaymentClaim.IncorrectAdditionalInformationCode,
          additionalDetails = "foo bar",
          reimbursementClaims = OrderedMap.from(
            Seq(
              exampleMrn        -> Map(TaxCode.A00 -> BigDecimal("1234.56")),
              anotherExampleMrn -> Map(TaxCode.A30 -> BigDecimal("12.34"), TaxCode.A90 -> BigDecimal("123456"))
            )
          ),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences,
          newEoriAndDan = None
        )

      val log =
        JourneyLog.apply(output, "123EORI", Some("REF-1234"), TestClaimAnalytics(emailAddressHasChanged = false))

      log.journeyType                           shouldBe "overpayments"
      log.journeyVariant                        shouldBe "multiple"
      log.numberOfMultipleMRNs                  shouldBe Some(2)
      log.claimantType                          shouldBe "Declarant"
      log.consigneeIsDeclarant                  shouldBe true
      log.payeeType                             shouldBe "Declarant"
      log.basisOfClaim                          shouldBe Some("IncorrectAdditionalInformationCode")
      log.basisOfClaimSpecialCircumstances      shouldBe None
      log.methodOfDisposal                      shouldBe None
      log.reasonForSecurity                     shouldBe None
      log.temporaryAdmissionMethodsOfDisposal   shouldBe None
      log.reimbursementMethod                   shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold                shouldBe "6"
      log.claimedDuties                         shouldBe Seq("A00", "A30", "A90")
      log.numberOfClaimedDuties                 shouldBe 3
      log.uploads.numberOfEvidenceFilesAttached shouldBe 2
      log.uploads.documentTypesAttached         shouldBe Seq("CommercialInvoice", "Other")
      log.uploads.fileTypesAttached             shouldBe Seq("application/pdf", "image/jpeg")
      log.uploads.fileSizes                     shouldBe Seq(567L, 1234L)
      log.uploads.scheduleFileType              shouldBe None
      log.uploads.scheduleFileSize              shouldBe None
      log.caseNumber                            shouldBe Some("REF-1234")
      log.userHash                              shouldBe "931058e4"
      log.changes.emailAddress                  shouldBe false
      log.changes.contactName                   shouldBe true
      log.changes.phoneNumber                   shouldBe true
      log.changes.contactAddress                shouldBe true
      log.changes.bankAccount                   shouldBe true
      log.journeyDurationSeconds                shouldBe 12345
    }

    "log OverpaymentsScheduledClaim.Output" in {
      val output: OverpaymentsScheduledClaim.Output =
        OverpaymentsScheduledClaim.Output(
          movementReferenceNumber = exampleMrn,
          scheduledDocument = exampleScheduledDocument,
          claimantType = ClaimantType.User,
          payeeType = PayeeType.Consignee,
          displayPayeeType = PayeeType.Consignee,
          claimantInformation = exampleClaimantInformation,
          basisOfClaim = BasisOfOverpaymentClaim.PersonalEffects,
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
          supportingEvidences = exampleSupportingEvidences,
          newEoriAndDan = None
        )

      val log =
        JourneyLog.apply(output, "123EORI", Some("REF-1234"), TestClaimAnalytics(contactNameHasChanged = false))

      log.journeyType                           shouldBe "overpayments"
      log.journeyVariant                        shouldBe "scheduled"
      log.numberOfMultipleMRNs                  shouldBe None
      log.claimantType                          shouldBe "User"
      log.consigneeIsDeclarant                  shouldBe true
      log.payeeType                             shouldBe "Consignee"
      log.basisOfClaim                          shouldBe Some("PersonalEffects")
      log.basisOfClaimSpecialCircumstances      shouldBe None
      log.methodOfDisposal                      shouldBe None
      log.reasonForSecurity                     shouldBe None
      log.temporaryAdmissionMethodsOfDisposal   shouldBe None
      log.reimbursementMethod                   shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold                shouldBe "6"
      log.claimedDuties                         shouldBe Seq("A00", "A30")
      log.numberOfClaimedDuties                 shouldBe 2
      log.uploads.numberOfEvidenceFilesAttached shouldBe 2
      log.uploads.documentTypesAttached         shouldBe Seq("CommercialInvoice", "Other")
      log.uploads.fileTypesAttached             shouldBe Seq("application/pdf", "image/jpeg")
      log.uploads.fileSizes                     shouldBe Seq(567L, 1234L)
      log.uploads.scheduleFileType              shouldBe Some("image/png")
      log.uploads.scheduleFileSize              shouldBe Some(9876L)
      log.caseNumber                            shouldBe Some("REF-1234")
      log.userHash                              shouldBe "931058e4"
      log.changes.emailAddress                  shouldBe true
      log.changes.contactName                   shouldBe false
      log.changes.phoneNumber                   shouldBe true
      log.changes.contactAddress                shouldBe true
      log.changes.bankAccount                   shouldBe true
      log.journeyDurationSeconds                shouldBe 12345
    }

    "log RejectedGoodsSingleClaim.Output" in {
      val output: RejectedGoodsSingleClaim.Output =
        RejectedGoodsSingleClaim.Output(
          movementReferenceNumber = exampleMrn,
          claimantType = ClaimantType.Consignee,
          payeeType = PayeeType.Consignee,
          displayPayeeType = PayeeType.Consignee,
          claimantInformation = exampleClaimantInformation,
          basisOfClaim = BasisOfRejectedGoodsClaim.DamagedBeforeClearance,
          basisOfClaimSpecialCircumstances = Some("BasisOfClaimSpecialCircumstances"),
          methodOfDisposal = MethodOfDisposal.Destruction,
          detailsOfRejectedGoods = "foo bar",
          inspectionDate = InspectionDate(LocalDate.now()),
          inspectionAddress = exampleInspectionAddress,
          reimbursements = Seq(
            Reimbursement(
              TaxCode.A00,
              BigDecimal("1234.56"),
              ReimbursementMethod.CurrentMonthAdjustment,
              BigDecimal("34.56")
            ),
            Reimbursement(
              TaxCode.A20,
              BigDecimal("12.34"),
              ReimbursementMethod.CurrentMonthAdjustment,
              BigDecimal("34.56")
            ),
            Reimbursement(
              TaxCode.A90,
              BigDecimal("12.345"),
              ReimbursementMethod.CurrentMonthAdjustment,
              BigDecimal("34.56")
            )
          ),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences
        )

      val log =
        JourneyLog.apply(output, "123EORI", Some("REF-1234"), TestClaimAnalytics(phoneNumberHasChanged = false))

      log.journeyType                           shouldBe "rejectedgoods"
      log.journeyVariant                        shouldBe "single"
      log.numberOfMultipleMRNs                  shouldBe None
      log.claimantType                          shouldBe "Consignee"
      log.consigneeIsDeclarant                  shouldBe true
      log.payeeType                             shouldBe "Consignee"
      log.basisOfClaim                          shouldBe Some("DamagedBeforeClearance")
      log.basisOfClaimSpecialCircumstances      shouldBe Some("BasisOfClaimSpecialCircumstances")
      log.methodOfDisposal                      shouldBe Some("Destruction")
      log.reasonForSecurity                     shouldBe None
      log.temporaryAdmissionMethodsOfDisposal   shouldBe None
      log.reimbursementMethod                   shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold                shouldBe "4"
      log.claimedDuties                         shouldBe Seq("A00", "A20", "A90")
      log.numberOfClaimedDuties                 shouldBe 3
      log.uploads.numberOfEvidenceFilesAttached shouldBe 2
      log.uploads.documentTypesAttached         shouldBe Seq("CommercialInvoice", "Other")
      log.uploads.fileTypesAttached             shouldBe Seq("application/pdf", "image/jpeg")
      log.uploads.fileSizes                     shouldBe Seq(567L, 1234L)
      log.uploads.scheduleFileType              shouldBe None
      log.uploads.scheduleFileSize              shouldBe None
      log.caseNumber                            shouldBe Some("REF-1234")
      log.userHash                              shouldBe "931058e4"
      log.changes.emailAddress                  shouldBe true
      log.changes.contactName                   shouldBe true
      log.changes.phoneNumber                   shouldBe false
      log.changes.contactAddress                shouldBe true
      log.changes.bankAccount                   shouldBe true
      log.journeyDurationSeconds                shouldBe 12345
    }

    "log RejectedGoodsMultipleClaim.Output" in {
      val output: RejectedGoodsMultipleClaim.Output =
        RejectedGoodsMultipleClaim.Output(
          movementReferenceNumbers = Seq(exampleMrn, anotherExampleMrn),
          claimantType = ClaimantType.Consignee,
          payeeType = PayeeType.Consignee,
          displayPayeeType = PayeeType.Consignee,
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

      val log =
        JourneyLog.apply(output, "123EORI", Some("REF-1234"), TestClaimAnalytics(contactAddressHasChanged = false))

      log.journeyType                           shouldBe "rejectedgoods"
      log.journeyVariant                        shouldBe "multiple"
      log.numberOfMultipleMRNs                  shouldBe Some(2)
      log.claimantType                          shouldBe "Consignee"
      log.consigneeIsDeclarant                  shouldBe true
      log.payeeType                             shouldBe "Consignee"
      log.basisOfClaim                          shouldBe Some("DamagedBeforeClearance")
      log.basisOfClaimSpecialCircumstances      shouldBe Some("BasisOfClaimSpecialCircumstances")
      log.methodOfDisposal                      shouldBe Some("Destruction")
      log.reasonForSecurity                     shouldBe None
      log.temporaryAdmissionMethodsOfDisposal   shouldBe None
      log.reimbursementMethod                   shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold                shouldBe "6"
      log.claimedDuties                         shouldBe Seq("A00", "A30", "A90")
      log.numberOfClaimedDuties                 shouldBe 3
      log.uploads.numberOfEvidenceFilesAttached shouldBe 2
      log.uploads.documentTypesAttached         shouldBe Seq("CommercialInvoice", "Other")
      log.uploads.fileTypesAttached             shouldBe Seq("application/pdf", "image/jpeg")
      log.uploads.fileSizes                     shouldBe Seq(567L, 1234L)
      log.uploads.scheduleFileType              shouldBe None
      log.uploads.scheduleFileSize              shouldBe None
      log.caseNumber                            shouldBe Some("REF-1234")
      log.userHash                              shouldBe "931058e4"
      log.changes.emailAddress                  shouldBe true
      log.changes.contactName                   shouldBe true
      log.changes.phoneNumber                   shouldBe true
      log.changes.contactAddress                shouldBe false
      log.changes.bankAccount                   shouldBe true
      log.journeyDurationSeconds                shouldBe 12345
    }

    "log RejectedGoodsScheduledClaim.Output" in {
      val output: RejectedGoodsScheduledClaim.Output =
        RejectedGoodsScheduledClaim.Output(
          movementReferenceNumber = exampleMrn,
          scheduledDocument = exampleScheduledDocument,
          claimantType = ClaimantType.User,
          payeeType = PayeeType.Consignee,
          displayPayeeType = PayeeType.Consignee,
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
                  TaxCode.A80 -> AmountPaidWithCorrect(BigDecimal("200000"), BigDecimal("1234.56"))
                )
              ),
              DutyType.EuDuty -> SortedMap.from(
                Seq(
                  TaxCode.A90 -> AmountPaidWithCorrect(BigDecimal("2000"), BigDecimal("12.34"))
                )
              )
            )
          ),
          reimbursementMethod = ReimbursementMethod.BankAccountTransfer,
          bankAccountDetails = Some(exampleBankAccountDetails),
          supportingEvidences = exampleSupportingEvidences
        )

      val log =
        JourneyLog.apply(output, "123EORI", Some("REF-1234"), TestClaimAnalytics(bankAccountHasChanged = false))

      log.journeyType                           shouldBe "rejectedgoods"
      log.journeyVariant                        shouldBe "scheduled"
      log.numberOfMultipleMRNs                  shouldBe None
      log.claimantType                          shouldBe "User"
      log.consigneeIsDeclarant                  shouldBe true
      log.payeeType                             shouldBe "Consignee"
      log.basisOfClaim                          shouldBe Some("DamagedBeforeClearance")
      log.basisOfClaimSpecialCircumstances      shouldBe Some("BasisOfClaimSpecialCircumstances")
      log.methodOfDisposal                      shouldBe Some("Destruction")
      log.reasonForSecurity                     shouldBe None
      log.temporaryAdmissionMethodsOfDisposal   shouldBe None
      log.reimbursementMethod                   shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold                shouldBe "6"
      log.claimedDuties                         shouldBe Seq("A80", "A90")
      log.numberOfClaimedDuties                 shouldBe 2
      log.uploads.numberOfEvidenceFilesAttached shouldBe 2
      log.uploads.documentTypesAttached         shouldBe Seq("CommercialInvoice", "Other")
      log.uploads.fileTypesAttached             shouldBe Seq("application/pdf", "image/jpeg")
      log.uploads.fileSizes                     shouldBe Seq(567L, 1234L)
      log.uploads.scheduleFileType              shouldBe Some("image/png")
      log.uploads.scheduleFileSize              shouldBe Some(9876L)
      log.caseNumber                            shouldBe Some("REF-1234")
      log.userHash                              shouldBe "931058e4"
      log.changes.emailAddress                  shouldBe true
      log.changes.contactName                   shouldBe true
      log.changes.phoneNumber                   shouldBe true
      log.changes.contactAddress                shouldBe true
      log.changes.bankAccount                   shouldBe false
      log.journeyDurationSeconds                shouldBe 12345
    }

    "log SecuritiesClaim.Output" in {
      val output: SecuritiesClaim.Output =
        SecuritiesClaim.Output(
          movementReferenceNumber = exampleMrn,
          claimantType = ClaimantType.Consignee,
          payeeType = Some(PayeeType.Consignee),
          displayPayeeType = Some(PayeeType.Consignee),
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
          temporaryAdmissionMethodsOfDisposal = Some(List(TemporaryAdmissionMethodOfDisposal.DeclaredToEndUse)),
          exportMovementReferenceNumber = Some(Seq(anotherExampleMrn))
        )

      val log = JourneyLog.apply(output, "123EORI", Some("REF-1234"), TestClaimAnalytics())

      log.journeyType                           shouldBe "securities"
      log.journeyVariant                        shouldBe "single"
      log.numberOfMultipleMRNs                  shouldBe None
      log.claimantType                          shouldBe "Consignee"
      log.consigneeIsDeclarant                  shouldBe true
      log.payeeType                             shouldBe "Consignee"
      log.basisOfClaim                          shouldBe None
      log.basisOfClaimSpecialCircumstances      shouldBe None
      log.methodOfDisposal                      shouldBe None
      log.reasonForSecurity                     shouldBe Some("MissingLicenseQuota")
      log.temporaryAdmissionMethodsOfDisposal   shouldBe Some(List("DeclaredToEndUse"))
      log.reimbursementMethod                   shouldBe "BankAccountTransfer"
      log.claimedAmountThreshold                shouldBe "5"
      log.claimedDuties                         shouldBe Seq("A30", "A45", "A90")
      log.numberOfClaimedDuties                 shouldBe 3
      log.uploads.numberOfEvidenceFilesAttached shouldBe 2
      log.uploads.documentTypesAttached         shouldBe Seq("CommercialInvoice", "Other")
      log.uploads.fileTypesAttached             shouldBe Seq("application/pdf", "image/jpeg")
      log.uploads.fileSizes                     shouldBe Seq(567L, 1234L)
      log.uploads.scheduleFileType              shouldBe None
      log.uploads.scheduleFileSize              shouldBe None
      log.caseNumber                            shouldBe Some("REF-1234")
      log.userHash                              shouldBe "931058e4"
      log.changes.emailAddress                  shouldBe true
      log.changes.contactName                   shouldBe true
      log.changes.phoneNumber                   shouldBe true
      log.changes.contactAddress                shouldBe true
      log.changes.bankAccount                   shouldBe true
      log.journeyDurationSeconds                shouldBe 12345
    }

  }
}
