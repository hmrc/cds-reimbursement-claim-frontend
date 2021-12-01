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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._

import java.time.Instant
import java.time.LocalDateTime

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._

import java.time.Instant
import java.time.LocalDateTime
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import java.time.LocalDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
trait RejectedGoodsSingleJourneyTestData {

  val exampleEori: Eori           = IdGen.genEori.sample.get
  val anotherExampleEori: Eori    = IdGen.genEori.sample.get
  val yetAnotherExampleEori: Eori = IdGen.genEori.sample.get

  val exampleEoriAsString: String = exampleEori.value

  val exampleMrn: MRN            = IdGen.genMRN.sample.get
  val exampleMrnAsString: String = exampleMrn.value

  val emptyJourney = RejectedGoodsSingleJourney.empty(exampleEori)

  val uploadDocument     = buildUploadDocument("foo")
  val uploadDocumentJson = buildUploadDocumentJson("foo")

  def tryBuildRejectedGoodsSingleJourney(
    userEoriNumber: Eori,
    mrn: MRN,
    displayDeclaration: DisplayDeclaration,
    basisOfClaim: BasisOfRejectedGoodsClaim,
    detailsOfRejectedGoods: String,
    specialCircumstancesDetails: String,
    inspectionDate: LocalDate,
    inspectionAddress: InspectionAddress,
    methodOfDisposal: MethodOfDisposal,
    reimbursementClaims: Seq[(TaxCode, BigDecimal, Boolean)],
    reimbursementMethod: ReimbursementMethodAnswer,
    supportingEvidences: Seq[(String, DocumentTypeRejectedGoods)],
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None
  ): Either[String, RejectedGoodsSingleJourney] = {
    val taxCodes: Seq[TaxCode]                                                              =
      reimbursementClaims.map(_._1)
    val taxCodesWithReimbursementAmount: Seq[(TaxCode, BigDecimal)]                         =
      reimbursementClaims.map(e => (e._1, e._2))
    val uploadedDocuments: Seq[UploadDocument]                                              =
      supportingEvidences.map(_._1).map(buildUploadDocument)
    val upscanReferencesWithDocumentType: Seq[(UploadReference, DocumentTypeRejectedGoods)] =
      uploadedDocuments.map(_.uploadReference).zip(supportingEvidences.map(_._2))

    def submitAmountForReimbursement(journey: RejectedGoodsSingleJourney)(
      taxCodesWithReimbursementAmount: (TaxCode, BigDecimal)
    ): Either[String, RejectedGoodsSingleJourney] =
      journey.submitAmountForReimbursement(taxCodesWithReimbursementAmount._1, taxCodesWithReimbursementAmount._2)

    def submitUploadedDocument(journey: RejectedGoodsSingleJourney)(
      uploadDocument: UploadDocument
    ): RejectedGoodsSingleJourney =
      journey.submitUploadedDocument(uploadDocument)

    def submitDocumentType(journey: RejectedGoodsSingleJourney)(
      uploadReferenceWithDocumentType: (UploadReference, DocumentTypeRejectedGoods)
    ): Either[String, RejectedGoodsSingleJourney] =
      journey.submitDocumentType(uploadReferenceWithDocumentType._1, uploadReferenceWithDocumentType._2)

    RejectedGoodsSingleJourney
      .empty(userEoriNumber)
      .submitMovementReferenceNumber(mrn)
      .submitDisplayDeclaration(displayDeclaration)
      .tryWhenDefined(consigneeEoriNumber)(_.submitConsigneeEoriNumber _)
      .flatMapWhenDefined(declarantEoriNumber)(_.submitDeclarantEoriNumber _)
      .mapWhenDefined(contactDetails)(_.submitContactDetails _)
      .mapWhenDefined(contactAddress)(_.submitContactAddress _)
      .map(_.submitBasisOfClaim(basisOfClaim))
      .flatMapWhen(basisOfClaim == BasisOfRejectedGoodsClaim.SpecialCircumstances)(
        _.submitBasisOfClaimSpecialCircumstancesDetails(specialCircumstancesDetails)
      )
      .map(_.submitMethodOfDisposal(methodOfDisposal))
      .map(_.submitDetailsOfRejectedGoods(detailsOfRejectedGoods))
      .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(taxCodes))
      .flatMapEach(taxCodesWithReimbursementAmount, submitAmountForReimbursement)
      .flatMap(_.submitInspectionDate(inspectionDate))
      .map(_.submitInspectionAddress(inspectionAddress))
      .mapWhenDefined(bankAccountDetails)(_.submitBankAccountDetails _)
      .mapWhenDefined(bankAccountType)(_.submitBankAccountType _)
      .flatMapWhen(_.isAllSelectedDutiesAreCMAEligible)(
        _.submitReimbursementMethod(reimbursementMethod)
      )
      .mapEach(uploadedDocuments, submitUploadedDocument)
      .flatMapEach(upscanReferencesWithDocumentType, submitDocumentType)
  }

  def buildDisplayDeclaration(
    id: String = "foo",
    declarantEORI: Eori,
    consigneeEORI: Option[Eori] = None,
    dutyDetails: Seq[(TaxCode, BigDecimal, Boolean)] = Seq.empty
  ): DisplayDeclaration = {
    val ndrcDetails: List[NdrcDetails] =
      dutyDetails.map { case (taxCode, paidAmount, cmaEligible) =>
        NdrcDetails(
          taxType = taxCode.value,
          amount = paidAmount.toString(),
          paymentMethod = s"payment-method-$id",
          paymentReference = s"payment-reference-$id",
          cmaEligible = if (cmaEligible) Some("1") else None
        )
      }.toList

    DisplayDeclaration {
      DisplayResponseDetail(
        declarationId = s"declaration-$id",
        acceptanceDate = "2021-10-11",
        declarantReferenceNumber = None,
        securityReason = None,
        btaDueDate = None,
        procedureCode = "procedure-code",
        btaSource = None,
        declarantDetails = DeclarantDetails(
          declarantEORI = declarantEORI.value,
          legalName = s"declarant-legal-name-$id",
          establishmentAddress = EstablishmentAddress(
            addressLine1 = s"declarant-address-line-1-$id",
            countryCode = s"declarant-country-code-$id"
          ),
          contactDetails = None
        ),
        consigneeDetails = consigneeEORI.map(eori =>
          ConsigneeDetails(
            consigneeEORI = eori.value,
            legalName = s"consignee-legal-name-$id",
            establishmentAddress = EstablishmentAddress(
              addressLine1 = s"consignee-address-line-1-$id",
              countryCode = s"consignee-country-code-$id"
            ),
            contactDetails = None
          )
        ),
        accountDetails = None,
        bankDetails = None,
        maskedBankDetails = None,
        ndrcDetails = if (ndrcDetails.isEmpty) None else Some(ndrcDetails)
      )
    }
  }

  def buildUploadDocument(id: String) = UploadDocument(
    uploadReference = UploadReference(s"upload-reference-$id"),
    upscanUploadMeta = UpscanUploadMeta(
      reference = s"reference-$id",
      uploadRequest = UploadRequest(
        href = s"upload-request-ref-$id",
        fields = Map("field-a" -> s"a-$id", "field-b" -> s"b-$id")
      )
    ),
    uploadedOn = LocalDateTime.parse("2007-12-03T10:15:30"),
    upscanSuccess = UpscanCallBack.UpscanSuccess(
      reference = s"upscan-reference-$id",
      fileStatus = s"upscan-file-status-$id",
      downloadUrl = s"upscan-download-url-$id",
      uploadDetails = UpscanCallBack.UploadDetails(
        fileName = s"file-name-$id",
        fileMimeType = s"application/$id",
        uploadTimestamp = Instant.ofEpochMilli(0L),
        checksum = "A" * 64,
        size = 1L
      )
    ),
    fileName = s"file-name-$id",
    documentType = None
  )

  def buildUploadDocumentJson(id: String): String =
    s"""{"uploadReference": "upload-reference-$id",
      |    "upscanUploadMeta": {
      |        "reference": "reference-$id",
      |        "uploadRequest": {
      |            "href": "upload-request-ref-$id",
      |            "fields": {
      |                "field-a": "a-$id",
      |                "field-b": "b-$id"
      |            }
      |        }
      |    },
      |    "uploadedOn": "2007-12-03T10:15:30",
      |    "upscanSuccess": {
      |        "reference": "upscan-reference-$id",
      |        "fileStatus": "upscan-file-status-$id",
      |        "downloadUrl": "upscan-download-url-$id",
      |        "uploadDetails": {
      |            "fileName": "file-name-$id",
      |            "fileMimeType": "application/$id",
      |            "uploadTimestamp": "1970-01-01T00:00:00Z",
      |            "checksum": "${"A" * 64}",
      |            "size": 1
      |        }
      |    },
      |    "fileName": "file-name-$id"
      |}""".stripMargin

}
