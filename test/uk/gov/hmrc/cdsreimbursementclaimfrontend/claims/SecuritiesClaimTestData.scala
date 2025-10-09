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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.claims

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.*

trait SecuritiesClaimTestData extends ClaimTestData {

  final val emptyClaim: SecuritiesClaim =
    SecuritiesClaim.empty(exampleEori)

  final def securitiesClaimWithMrnAndRfsAndDeclaration(rfs: ReasonForSecurity) = SecuritiesClaim
    .empty(exampleEori)
    .submitMovementReferenceNumber(exampleMrn)
    .submitReasonForSecurityAndDeclaration(
      rfs,
      buildSecuritiesDisplayDeclaration(
        securityReason = rfs.acc14Code,
        declarantContact = Some(exampleDeclarationContactDetails)
      )
    )
    .flatMap(_.submitClaimDuplicateCheckStatus(false))
    .getOrFail

  final def buildSecuritiesClaimReadyForSelectingSecurities(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesClaim = testParams match {
    case (mrn, rfs, decl) =>
      emptyClaim
        .submitMovementReferenceNumber(mrn)
        .submitReasonForSecurityAndDeclaration(rfs, decl)
        .flatMap(_.submitClaimDuplicateCheckStatus(false))
        .getOrFail
  }

  final def buildSecuritiesClaimReadyForIPR(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesClaim = testParams match {
    case (mrn, rfs, decl) =>
      emptyClaim
        .submitMovementReferenceNumber(mrn)
        .submitReasonForSecurityAndDeclaration(rfs, decl)
        .flatMap(_.submitClaimDuplicateCheckStatus(false))
        .getOrFail
  }

  final def buildSecuritiesClaimReadyForENU(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesClaim = testParams match {
    case (mrn, rfs, decl) =>
      emptyClaim
        .submitMovementReferenceNumber(mrn)
        .submitReasonForSecurityAndDeclaration(rfs, decl)
        .flatMap(_.submitClaimDuplicateCheckStatus(false))
        .getOrFail
  }

  final def buildSecuritiesClaimReadyForNidac(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesClaim = testParams match {
    case (mrn, rfs, decl) =>
      emptyClaim
        .submitMovementReferenceNumber(mrn)
        .submitReasonForSecurityAndDeclaration(rfs, decl)
        .flatMap(_.submitClaimDuplicateCheckStatus(false))
        .getOrFail
  }

  final def buildSecuritiesClaimWithDocumentTypeSelected(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, UploadDocumentType)
  ): SecuritiesClaim = testParams match {
    case (mrn, rfs, decl, dt) =>
      emptyClaim
        .submitMovementReferenceNumber(mrn)
        .submitReasonForSecurityAndDeclaration(rfs, decl)
        .flatMap(_.submitClaimDuplicateCheckStatus(false))
        .flatMap(_.submitDocumentTypeSelection(dt))
        .getOrFail
  }

  final def buildSecuritiesClaimWithSomeSecuritiesSelected(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesClaim = {
    val claim      = buildSecuritiesClaimReadyForSelectingSecurities(testParams)
    val depositIds = claim.getSecurityDepositIds
    claim
      .submitCheckDeclarationDetailsChangeMode(false)
      .selectSecurityDepositIds(depositIds.secondHalfNonEmpty)
      .getOrFail
  }

  final def buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposal(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesClaim = {
    val claim      = buildSecuritiesClaimReadyForSelectingSecurities(testParams)
    val depositIds = claim.getSecurityDepositIds
    claim
      .submitCheckDeclarationDetailsChangeMode(false)
      .selectSecurityDepositIds(depositIds.secondHalfNonEmpty)
      .flatMap(
        _.submitTemporaryAdmissionMethodsOfDisposal(
          List(TemporaryAdmissionMethodOfDisposal.ExportedInSingleOrMultipleShipments)
        )
      )
      .getOrFail
  }

  final def buildSecuritiesClaimWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
    exportMRNs: Seq[MRN]
  )(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesClaim = {
    val claim      = buildSecuritiesClaimReadyForSelectingSecurities(testParams)
    val depositIds = claim.getSecurityDepositIds
    claim
      .submitCheckDeclarationDetailsChangeMode(false)
      .selectSecurityDepositIds(depositIds.secondHalfNonEmpty)
      .flatMap(
        _.submitTemporaryAdmissionMethodsOfDisposal(
          List(TemporaryAdmissionMethodOfDisposal.ExportedInSingleOrMultipleShipments)
        )
      )
      .flatMapEach(
        exportMRNs.zipWithIndex,
        (j: SecuritiesClaim) =>
          { case (mrn: MRN, index: Int) =>
            j.submitExportMovementReferenceNumber(index, mrn)
          }: (((MRN, Int)) => Either[String, SecuritiesClaim])
      )
      .getOrFail
  }

  final def buildSecuritiesClaimWithSomeSecuritiesSelectedGeneratedMfd(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, TemporaryAdmissionMethodOfDisposal)
  ): SecuritiesClaim = testParams match {
    case (mrn: MRN, rfs: ReasonForSecurity, acc14: DisplayDeclaration, _: TemporaryAdmissionMethodOfDisposal) =>
      buildSecuritiesClaimWithSomeSecuritiesSelected((mrn, rfs, acc14))
  }

  final def buildSecuritiesClaimWithSomeSecuritiesSelectedWithMehodOfDisposal(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, TemporaryAdmissionMethodOfDisposal)
  ): SecuritiesClaim = testParams match {
    case (mrn: MRN, rfs: ReasonForSecurity, acc14: DisplayDeclaration, mfd: TemporaryAdmissionMethodOfDisposal) =>
      buildSecuritiesClaimWithSomeSecuritiesSelected((mrn, rfs, acc14))
        .submitTemporaryAdmissionMethodsOfDisposal(List(mfd))
        .getOrFail
  }
  final def buildSecuritiesClaimInChangeDeclarationDetailsMode(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesClaim =
    buildSecuritiesClaimWithSomeSecuritiesSelected(testParams)
      .submitCheckDeclarationDetailsChangeMode(true)

  final def buildSecuritiesClaimWithDutiesPartiallySelected(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal, BigDecimal)])
  ): SecuritiesClaim = testParams match {
    case (mrn, rfs, decl, reclaims) =>
      val availableDepositIds: Seq[String] = decl.getSecurityDepositIds.get

      val depositIdsWithSomeDutiesSelected: Seq[String] = reclaims.map(_._1).distinct

      val depositIdsWithoutDutiesSelected: Seq[String] =
        availableDepositIds.filterNot(depositIdsWithSomeDutiesSelected.contains).halfNonEmpty

      val taxCodesPerDepositId: Seq[(String, Seq[TaxCode])] =
        reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, _) => tc }).toSeq

      buildSecuritiesClaimReadyForSelectingSecurities((mrn, rfs, decl))
        .flatMapEach(
          depositIdsWithSomeDutiesSelected ++ depositIdsWithoutDutiesSelected,
          (claim: SecuritiesClaim) => claim.selectSecurityDepositId(_)
        )
        .flatMapEach(
          taxCodesPerDepositId,
          (claim: SecuritiesClaim) =>
            (args: (String, Seq[TaxCode])) =>
              claim
                .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2)
        )
        .getOrFail
  }

  final def buildSecuritiesClaimReadyForEnteringClaimAmounts(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal, BigDecimal)])
  ): SecuritiesClaim = testParams match {
    case (mrn, rfs, decl, reclaims) =>
      val depositIds: Seq[String]                           = reclaims.map(_._1).distinct
      val taxCodesPerDepositId: Seq[(String, Seq[TaxCode])] =
        reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, _) => tc }).toSeq

      buildSecuritiesClaimReadyForSelectingSecurities((mrn, rfs, decl))
        .flatMapEach(
          depositIds,
          (claim: SecuritiesClaim) => claim.selectSecurityDepositId(_)
        )
        .flatMapEach(
          taxCodesPerDepositId,
          (claim: SecuritiesClaim) =>
            (args: (String, Seq[TaxCode])) =>
              claim
                .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2)
        )
        .getOrFail
  }

  final def buildSecuritiesClaimWithClaimsEntered(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal, BigDecimal)])
  ): SecuritiesClaim = testParams match {
    case (mrn, rfs, decl, reclaims) =>
      val depositIds = reclaims.map(_._1).distinct

      buildSecuritiesClaimReadyForSelectingSecurities((mrn, rfs, decl))
        .flatMapEach(
          depositIds,
          (claim: SecuritiesClaim) => claim.selectSecurityDepositId(_)
        )
        .flatMapEach(
          reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, amount) => (tc, amount) }).toSeq,
          (claim: SecuritiesClaim) =>
            (args: (String, Seq[(TaxCode, BigDecimal)])) =>
              claim
                .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                .flatMapEach(
                  args._2,
                  (claim: SecuritiesClaim) =>
                    (args2: (TaxCode, BigDecimal)) => claim.submitCorrectAmount(args._1, args2._1, args2._2)
                )
        )
        .getOrFail
  }

}
