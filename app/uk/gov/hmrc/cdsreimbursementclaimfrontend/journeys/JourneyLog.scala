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

import play.api.libs.json.Json
import play.api.libs.json.OFormat
import play.api.Logger

final case class JourneyLog(
  journeyType: String,
  journeyVariant: String,
  numberOfMultipleMRNs: Option[Int] = None,
  claimantType: String,
  basisOfClaim: Option[String] = None,
  basisOfClaimSpecialCircumstances: Option[String] = None,
  methodOfDisposal: Option[String] = None,
  whetherNorthernIreland: Option[Boolean] = None,
  reasonForSecurity: Option[String] = None,
  temporaryAdmissionMethodOfDisposal: Option[String] = None,
  reimbursementMethod: String,
  claimedAmountThreshold: String,
  claimedDuties: Seq[String],
  numberOfClaimedDuties: Int,
  numberOfEvidenceFilesAttached: Int,
  documentTypesAttached: Seq[String],
  fileTypesAttached: Seq[String],
  fileSizes: Seq[Long],
  scheduleFileType: Option[String] = None,
  scheduleFileSize: Option[Long] = None
) {

  def logInfo(): Unit = {
    val jsonString = Json.stringify(JourneyLog.formatter.writes(this))
    Logger(getClass()).info(s"json$jsonString")
  }
}

object JourneyLog {

  def apply(output: OverpaymentsSingleJourney.Output): JourneyLog =
    JourneyLog(
      journeyType = "overpayments",
      journeyVariant = "single",
      numberOfMultipleMRNs = None,
      claimantType = output.claimantType.toString,
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = None,
      methodOfDisposal = None,
      whetherNorthernIreland = Some(output.whetherNorthernIreland),
      reasonForSecurity = None,
      temporaryAdmissionMethodOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursementClaims.map(_._2).sum),
      claimedDuties = output.reimbursementClaims.keySet.map(_.toString()).toSeq.sorted,
      numberOfClaimedDuties = output.reimbursementClaims.size,
      numberOfEvidenceFilesAttached = output.supportingEvidences.size,
      documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
      fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
      fileSizes = output.supportingEvidences.map(_.size).sorted,
      scheduleFileType = None,
      scheduleFileSize = None
    )

  def apply(output: OverpaymentsMultipleJourney.Output): JourneyLog =
    JourneyLog(
      journeyType = "overpayments",
      journeyVariant = "multiple",
      numberOfMultipleMRNs = Some(output.movementReferenceNumbers.size),
      claimantType = output.claimantType.toString,
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = None,
      methodOfDisposal = None,
      whetherNorthernIreland = Some(output.whetherNorthernIreland),
      reasonForSecurity = None,
      temporaryAdmissionMethodOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursementClaims.map(_._2.map(_._2).sum).sum),
      claimedDuties = output.reimbursementClaims.map(_._2.keySet.map(_.toString)).flatten.toSeq.sorted,
      numberOfClaimedDuties = output.reimbursementClaims.map(_._2.size).sum,
      numberOfEvidenceFilesAttached = output.supportingEvidences.size,
      documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
      fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
      fileSizes = output.supportingEvidences.map(_.size).sorted,
      scheduleFileType = None,
      scheduleFileSize = None
    )

  def apply(output: OverpaymentsScheduledJourney.Output): JourneyLog =
    JourneyLog(
      journeyType = "overpayments",
      journeyVariant = "scheduled",
      numberOfMultipleMRNs = None,
      claimantType = output.claimantType.toString,
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = None,
      methodOfDisposal = None,
      whetherNorthernIreland = Some(output.whetherNorthernIreland),
      reasonForSecurity = None,
      temporaryAdmissionMethodOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursementClaims.map(_._2.map(_._2.refundAmount).sum).sum),
      claimedDuties = output.reimbursementClaims.map(_._2.keySet.map(_.toString)).flatten.toSeq.sorted,
      numberOfClaimedDuties = output.reimbursementClaims.map(_._2.size).sum,
      numberOfEvidenceFilesAttached = output.supportingEvidences.size,
      documentTypesAttached = output.supportingEvidences.map(_.documentType.toString).sorted,
      fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
      fileSizes = output.supportingEvidences.map(_.size).sorted,
      scheduleFileType = Some(output.scheduledDocument.fileMimeType),
      scheduleFileSize = Some(output.scheduledDocument.size)
    )

  def apply(output: RejectedGoodsSingleJourney.Output): JourneyLog =
    JourneyLog(
      journeyType = "rejectedgoods",
      journeyVariant = "single",
      numberOfMultipleMRNs = None,
      claimantType = output.claimantType.toString,
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = output.basisOfClaimSpecialCircumstances,
      methodOfDisposal = Some(output.methodOfDisposal.toString),
      whetherNorthernIreland = None,
      reasonForSecurity = None,
      temporaryAdmissionMethodOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursementClaims.map(_._2).sum),
      claimedDuties = output.reimbursementClaims.keySet.map(_.toString()).toSeq.sorted,
      numberOfClaimedDuties = output.reimbursementClaims.size,
      numberOfEvidenceFilesAttached = output.supportingEvidences.size,
      documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
      fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
      fileSizes = output.supportingEvidences.map(_.size).sorted,
      scheduleFileType = None,
      scheduleFileSize = None
    )

  def apply(output: RejectedGoodsMultipleJourney.Output): JourneyLog =
    JourneyLog(
      journeyType = "rejectedgoods",
      journeyVariant = "multiple",
      numberOfMultipleMRNs = Some(output.movementReferenceNumbers.size),
      claimantType = output.claimantType.toString,
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = output.basisOfClaimSpecialCircumstances,
      methodOfDisposal = Some(output.methodOfDisposal.toString),
      whetherNorthernIreland = None,
      reasonForSecurity = None,
      temporaryAdmissionMethodOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursementClaims.map(_._2.map(_._2).sum).sum),
      claimedDuties = output.reimbursementClaims.map(_._2.keySet.map(_.toString)).flatten.toSeq.sorted,
      numberOfClaimedDuties = output.reimbursementClaims.map(_._2.size).sum,
      numberOfEvidenceFilesAttached = output.supportingEvidences.size,
      documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
      fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
      fileSizes = output.supportingEvidences.map(_.size).sorted,
      scheduleFileType = None,
      scheduleFileSize = None
    )

  def apply(output: RejectedGoodsScheduledJourney.Output): JourneyLog =
    JourneyLog(
      journeyType = "rejectedgoods",
      journeyVariant = "scheduled",
      numberOfMultipleMRNs = None,
      claimantType = output.claimantType.toString,
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = output.basisOfClaimSpecialCircumstances,
      methodOfDisposal = Some(output.methodOfDisposal.toString),
      whetherNorthernIreland = None,
      reasonForSecurity = None,
      temporaryAdmissionMethodOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursementClaims.map(_._2.map(_._2.refundAmount).sum).sum),
      claimedDuties = output.reimbursementClaims.map(_._2.keySet.map(_.toString)).flatten.toSeq.sorted,
      numberOfClaimedDuties = output.reimbursementClaims.map(_._2.size).sum,
      numberOfEvidenceFilesAttached = output.supportingEvidences.size,
      documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
      fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
      fileSizes = output.supportingEvidences.map(_.size).sorted,
      scheduleFileType = Some(output.scheduledDocument.fileMimeType),
      scheduleFileSize = Some(output.scheduledDocument.size)
    )

  def apply(output: SecuritiesJourney.Output): JourneyLog =
    JourneyLog(
      journeyType = "securities",
      journeyVariant = "single",
      numberOfMultipleMRNs = None,
      claimantType = output.claimantType.toString,
      basisOfClaim = None,
      basisOfClaimSpecialCircumstances = None,
      methodOfDisposal = None,
      whetherNorthernIreland = None,
      reasonForSecurity = Some(output.reasonForSecurity.toString),
      temporaryAdmissionMethodOfDisposal = output.temporaryAdmissionMethodOfDisposal.map(_.toString),
      reimbursementMethod = if (output.bankAccountDetails.isDefined) "BankAccountTransfer" else "Other",
      claimedAmountThreshold = threshold(output.securitiesReclaims.map(_._2.map(_._2).sum).sum),
      claimedDuties = output.securitiesReclaims.map(_._2.keySet.map(_.toString)).flatten.toSeq.sorted,
      numberOfClaimedDuties = output.securitiesReclaims.map(_._2.size).sum,
      numberOfEvidenceFilesAttached = output.supportingEvidences.size,
      documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
      fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
      fileSizes = output.supportingEvidences.map(_.size).sorted,
      scheduleFileType = None,
      scheduleFileSize = None
    )

  private def threshold(amount: BigDecimal): String =
    if (amount >= 1000000000) "10"
    else if (amount >= 100000000) "9"
    else if (amount >= 10000000) "8"
    else if (amount >= 1000000) "7"
    else if (amount >= 100000) "6"
    else if (amount >= 10000) "5"
    else if (amount >= 1000) "4"
    else if (amount >= 100) "3"
    else if (amount >= 10) "2"
    else "1"

  implicit val formatter: OFormat[JourneyLog] = Json.format[JourneyLog]
}
