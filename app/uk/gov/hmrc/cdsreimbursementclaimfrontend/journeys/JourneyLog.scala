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

import play.api.Logger
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Hash

final case class JourneyLog(
  journeyType: String,
  journeyVariant: String,
  numberOfMultipleMRNs: Option[Int] = None,
  claimantType: String,
  consigneeIsDeclarant: Boolean,
  payeeType: String,
  basisOfClaim: Option[String] = None,
  basisOfClaimSpecialCircumstances: Option[String] = None,
  methodOfDisposal: Option[String] = None,
  reasonForSecurity: Option[String] = None,
  temporaryAdmissionMethodsOfDisposal: Option[List[String]] = None,
  reimbursementMethod: String,
  claimedAmountThreshold: String,
  claimedDuties: Seq[String],
  numberOfClaimedDuties: Int,
  uploads: JourneyLog.Uploads,
  changes: JourneyLog.Changes,
  userHash: String,
  caseNumber: Option[String] = None
) {

  def logInfo(): JsObject = {
    val journeyLogJson = JourneyLog.formatter.writes(this)
    val jsonString     = Json.stringify(journeyLogJson)
    Logger(getClass()).info(s"json$jsonString")
    journeyLogJson
  }

  def logError(e: Throwable): JsObject = {
    val journeyLogJson = JourneyLog.formatter.writes(this).+("error" -> JsString(e.getMessage()))
    val jsonString     = Json.stringify(journeyLogJson)
    Logger(getClass()).error(s"json$jsonString")
    journeyLogJson
  }
}

object JourneyLog {

  final case class Uploads(
    numberOfEvidenceFilesAttached: Int,
    documentTypesAttached: Seq[String],
    fileTypesAttached: Seq[String],
    fileSizes: Seq[Long],
    scheduleFileType: Option[String] = None,
    scheduleFileSize: Option[Long] = None
  )

  final case class Changes(
    emailAddress: Boolean = false,
    contactName: Boolean = false,
    phoneNumber: Boolean = false,
    contactAddress: Boolean = false,
    bankAccount: Boolean = false
  )

  object Changes {

    def from(analytics: JourneyAnalytics): Changes =
      Changes(
        emailAddress = analytics.emailAddressHasChanged,
        contactName = analytics.contactNameHasChanged,
        phoneNumber = analytics.phoneNumberHasChanged,
        contactAddress = analytics.contactAddressHasChanged,
        bankAccount = analytics.bankAccountHasChanged
      )
  }

  def apply(
    output: OverpaymentsSingleJourney.Output,
    userEORI: String,
    caseNumber: Option[String],
    analytics: JourneyAnalytics
  ): JourneyLog =
    JourneyLog(
      journeyType = "overpayments",
      journeyVariant = "single",
      numberOfMultipleMRNs = None,
      claimantType = output.claimantType.toString(),
      consigneeIsDeclarant = analytics.declarantEoriMatchesConsignee,
      payeeType = output.payeeType.toString(),
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = None,
      methodOfDisposal = None,
      reasonForSecurity = None,
      temporaryAdmissionMethodsOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursements.map(_.amount).sum),
      claimedDuties = output.reimbursements.map(_.taxCode.toString).sorted,
      numberOfClaimedDuties = output.reimbursements.size,
      uploads = Uploads(
        numberOfEvidenceFilesAttached = output.supportingEvidences.size,
        documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
        fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
        fileSizes = output.supportingEvidences.map(_.size).sorted,
        scheduleFileType = None,
        scheduleFileSize = None
      ),
      changes = Changes.from(analytics),
      userHash = Hash(userEORI).take(8),
      caseNumber = caseNumber
    )

  def apply(
    output: OverpaymentsMultipleJourney.Output,
    userEORI: String,
    caseNumber: Option[String],
    analytics: JourneyAnalytics
  ): JourneyLog =
    JourneyLog(
      journeyType = "overpayments",
      journeyVariant = "multiple",
      numberOfMultipleMRNs = Some(output.movementReferenceNumbers.size),
      claimantType = output.claimantType.toString(),
      consigneeIsDeclarant = analytics.declarantEoriMatchesConsignee,
      payeeType = output.payeeType.toString(),
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = None,
      methodOfDisposal = None,
      reasonForSecurity = None,
      temporaryAdmissionMethodsOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursementClaims.map(_._2.map(_._2).sum).sum),
      claimedDuties = output.reimbursementClaims.map(_._2.keySet.map(_.toString)).flatten.toSeq.sorted,
      numberOfClaimedDuties = output.reimbursementClaims.map(_._2.size).sum,
      uploads = Uploads(
        numberOfEvidenceFilesAttached = output.supportingEvidences.size,
        documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
        fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
        fileSizes = output.supportingEvidences.map(_.size).sorted,
        scheduleFileType = None,
        scheduleFileSize = None
      ),
      changes = Changes.from(analytics),
      userHash = Hash(userEORI).take(8),
      caseNumber = caseNumber
    )

  def apply(
    output: OverpaymentsScheduledJourney.Output,
    userEORI: String,
    caseNumber: Option[String],
    analytics: JourneyAnalytics
  ): JourneyLog =
    JourneyLog(
      journeyType = "overpayments",
      journeyVariant = "scheduled",
      numberOfMultipleMRNs = None,
      claimantType = output.claimantType.toString(),
      consigneeIsDeclarant = analytics.declarantEoriMatchesConsignee,
      payeeType = output.payeeType.toString(),
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = None,
      methodOfDisposal = None,
      reasonForSecurity = None,
      temporaryAdmissionMethodsOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursementClaims.map(_._2.map(_._2.claimAmount).sum).sum),
      claimedDuties = output.reimbursementClaims.map(_._2.keySet.map(_.toString)).flatten.toSeq.sorted,
      numberOfClaimedDuties = output.reimbursementClaims.map(_._2.size).sum,
      uploads = Uploads(
        numberOfEvidenceFilesAttached = output.supportingEvidences.size,
        documentTypesAttached = output.supportingEvidences.map(_.documentType.toString).sorted,
        fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
        fileSizes = output.supportingEvidences.map(_.size).sorted,
        scheduleFileType = Some(output.scheduledDocument.fileMimeType),
        scheduleFileSize = Some(output.scheduledDocument.size)
      ),
      changes = Changes.from(analytics),
      userHash = Hash(userEORI).take(8),
      caseNumber = caseNumber
    )

  def apply(
    output: RejectedGoodsSingleJourney.Output,
    userEORI: String,
    caseNumber: Option[String],
    analytics: JourneyAnalytics
  ): JourneyLog =
    JourneyLog(
      journeyType = "rejectedgoods",
      journeyVariant = "single",
      numberOfMultipleMRNs = None,
      claimantType = output.claimantType.toString(),
      consigneeIsDeclarant = analytics.declarantEoriMatchesConsignee,
      payeeType = output.payeeType.toString(),
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = output.basisOfClaimSpecialCircumstances,
      methodOfDisposal = Some(output.methodOfDisposal.toString),
      reasonForSecurity = None,
      temporaryAdmissionMethodsOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursements.map(_.amount).sum),
      claimedDuties = output.reimbursements.map(_.taxCode.toString).sorted,
      numberOfClaimedDuties = output.reimbursements.size,
      uploads = Uploads(
        numberOfEvidenceFilesAttached = output.supportingEvidences.size,
        documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
        fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
        fileSizes = output.supportingEvidences.map(_.size).sorted,
        scheduleFileType = None,
        scheduleFileSize = None
      ),
      changes = Changes.from(analytics),
      userHash = Hash(userEORI).take(8),
      caseNumber = caseNumber
    )

  def apply(
    output: RejectedGoodsMultipleJourney.Output,
    userEORI: String,
    caseNumber: Option[String],
    analytics: JourneyAnalytics
  ): JourneyLog =
    JourneyLog(
      journeyType = "rejectedgoods",
      journeyVariant = "multiple",
      numberOfMultipleMRNs = Some(output.movementReferenceNumbers.size),
      claimantType = output.claimantType.toString(),
      consigneeIsDeclarant = analytics.declarantEoriMatchesConsignee,
      payeeType = output.payeeType.toString(),
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = output.basisOfClaimSpecialCircumstances,
      methodOfDisposal = Some(output.methodOfDisposal.toString),
      reasonForSecurity = None,
      temporaryAdmissionMethodsOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursementClaims.map(_._2.map(_._2).sum).sum),
      claimedDuties = output.reimbursementClaims.map(_._2.keySet.map(_.toString)).flatten.toSeq.sorted,
      numberOfClaimedDuties = output.reimbursementClaims.map(_._2.size).sum,
      uploads = Uploads(
        numberOfEvidenceFilesAttached = output.supportingEvidences.size,
        documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
        fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
        fileSizes = output.supportingEvidences.map(_.size).sorted,
        scheduleFileType = None,
        scheduleFileSize = None
      ),
      changes = Changes.from(analytics),
      userHash = Hash(userEORI).take(8),
      caseNumber = caseNumber
    )

  def apply(
    output: RejectedGoodsScheduledJourney.Output,
    userEORI: String,
    caseNumber: Option[String],
    analytics: JourneyAnalytics
  ): JourneyLog =
    JourneyLog(
      journeyType = "rejectedgoods",
      journeyVariant = "scheduled",
      numberOfMultipleMRNs = None,
      claimantType = output.claimantType.toString(),
      consigneeIsDeclarant = analytics.declarantEoriMatchesConsignee,
      payeeType = output.payeeType.toString(),
      basisOfClaim = Some(output.basisOfClaim.toString),
      basisOfClaimSpecialCircumstances = output.basisOfClaimSpecialCircumstances,
      methodOfDisposal = Some(output.methodOfDisposal.toString),
      reasonForSecurity = None,
      temporaryAdmissionMethodsOfDisposal = None,
      reimbursementMethod = output.reimbursementMethod.toString,
      claimedAmountThreshold = threshold(output.reimbursementClaims.map(_._2.map(_._2.claimAmount).sum).sum),
      claimedDuties = output.reimbursementClaims.map(_._2.keySet.map(_.toString)).flatten.toSeq.sorted,
      numberOfClaimedDuties = output.reimbursementClaims.map(_._2.size).sum,
      uploads = Uploads(
        numberOfEvidenceFilesAttached = output.supportingEvidences.size,
        documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
        fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
        fileSizes = output.supportingEvidences.map(_.size).sorted,
        scheduleFileType = Some(output.scheduledDocument.fileMimeType),
        scheduleFileSize = Some(output.scheduledDocument.size)
      ),
      changes = Changes.from(analytics),
      userHash = Hash(userEORI).take(8),
      caseNumber = caseNumber
    )

  def apply(
    output: SecuritiesJourney.Output,
    userEORI: String,
    caseNumber: Option[String],
    analytics: JourneyAnalytics
  ): JourneyLog =
    JourneyLog(
      journeyType = "securities",
      journeyVariant = "single",
      numberOfMultipleMRNs = None,
      claimantType = output.claimantType.toString(),
      consigneeIsDeclarant = analytics.declarantEoriMatchesConsignee,
      payeeType = output.payeeType.toString(),
      basisOfClaim = None,
      basisOfClaimSpecialCircumstances = None,
      methodOfDisposal = None,
      reasonForSecurity = Some(output.reasonForSecurity.toString),
      temporaryAdmissionMethodsOfDisposal = output.temporaryAdmissionMethodsOfDisposal.map { methods =>
        methods.map(_.toString)
      },
      reimbursementMethod = if output.bankAccountDetails.isDefined then "BankAccountTransfer" else "Other",
      claimedAmountThreshold = threshold(output.securitiesReclaims.map(_._2.map(_._2).sum).sum),
      claimedDuties = output.securitiesReclaims.map(_._2.keySet.map(_.toString)).flatten.toSeq.sorted,
      numberOfClaimedDuties = output.securitiesReclaims.map(_._2.size).sum,
      uploads = Uploads(
        numberOfEvidenceFilesAttached = output.supportingEvidences.size,
        documentTypesAttached = output.supportingEvidences.map(_.documentType.toString()).sorted,
        fileTypesAttached = output.supportingEvidences.map(_.fileMimeType).sorted,
        fileSizes = output.supportingEvidences.map(_.size).sorted,
        scheduleFileType = None,
        scheduleFileSize = None
      ),
      changes = Changes.from(analytics),
      userHash = Hash(userEORI).take(8),
      caseNumber = caseNumber
    )

  private def threshold(amount: BigDecimal): String =
    if amount >= 1000000000 then "10"
    else if amount >= 100000000 then "9"
    else if amount >= 10000000 then "8"
    else if amount >= 1000000 then "7"
    else if amount >= 100000 then "6"
    else if amount >= 10000 then "5"
    else if amount >= 1000 then "4"
    else if amount >= 100 then "3"
    else if amount >= 10 then "2"
    else "1"

  implicit val formatterUploads: OFormat[Uploads] = Json.format[Uploads]
  implicit val formatterChanges: OFormat[Changes] = Json.using[Json.WithDefaultValues].format[Changes]
  implicit val formatter: OFormat[JourneyLog]     = Json.format[JourneyLog]
}
