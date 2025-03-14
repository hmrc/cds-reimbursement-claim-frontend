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

import cats.syntax.eq.*
import play.api.data.format.Formatter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils

import java.lang
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZonedDateTime

trait JourneyTestData {

  implicit class EitherOps[A](val either: Either[String, A]) {
    def getOrFail(implicit pos: org.scalactic.source.Position): A =
      either.fold(
        error =>
          throw new Exception(
            s"Journey construction has failed because of $error at ${pos.fileName}:${pos.lineNumber}"
          ),
        identity
      )

    def expectFailure(expectedError: String)(implicit pos: org.scalactic.source.Position): Unit =
      either.fold(
        error =>
          if error === expectedError then ()
          else
            throw new Exception(
              s"Journey construction in ${pos.fileName}:${pos.lineNumber} has failed as expected, but error was different: $error"
            )
        ,
        _ =>
          throw new Exception(
            s"Expected failure but journey construction succeeded in ${pos.fileName}:${pos.lineNumber}"
          )
      )
  }

  implicit class SeqTestOps[A](val seq: Seq[A]) {
    def halfNonEmpty: Seq[A] =
      if seq.isEmpty then throw new lang.Error("Cannot shrink the sequence because is empty.")
      else if seq.size > 1 then seq.take(seq.size / 2)
      else seq

    def secondHalfNonEmpty: Seq[A] =
      if seq.isEmpty then throw new lang.Error("Cannot shrink the sequence because is empty.")
      else if seq.size > 1 then seq.drop(seq.length - seq.size / 2)
      else seq

    def headSeq: Seq[A] =
      seq.headOption.map(Seq(_)).getOrElse(Seq.empty)

    def takeExcept(other: Seq[A]): Seq[A] =
      seq.filterNot(other.contains(_))

    def otherThen(a: A): A =
      seq
        .find(_ != a)
        .getOrElse(throw new lang.Error("Cannot find other element."))
  }

  final val exampleEori: Eori           = IdGen.genEori.sample.get
  final val exampleDan: Dan             = IdGen.genDan.sample.get
  final val anotherExampleEori: Eori    = IdGen.genEori.sample.get
  final val yetAnotherExampleEori: Eori = IdGen.genEori.sample.get

  final val exampleEoriNewFormat = IdGen.genNewEoriFormat.sample.get

  final val exampleXIEori: Eori           = IdGen.genXiEori.sample.get
  final val anotherExampleXIEori: Eori    = IdGen.genXiEori.sample.get
  final val yetAnotherExampleXIEori: Eori = IdGen.genXiEori.sample.get

  final val exampleEoriAsString: String = exampleEori.value

  final val exampleMrn: MRN            = IdGen.genMRN.sample.get
  final val anotherExampleMrn: MRN     = IdGen.genMRN.sample.get
  final val exampleMrnAsString: String = exampleMrn.value

  final val exampleUploadedFile  = buildUploadDocument("foo")
  final val exampleUploadedFiles = Seq("foo", "zoo", "coo").map(buildUploadDocument)

  final val exampleDeclarationContactDetails: ContactDetails =
    Acc14Gen.genContactDetails.sample.get

  final val exampleContactDetails: MrnContactDetails =
    MrnContactDetails(
      fullName = "Foo Bar",
      emailAddress = Some(Email("foo@bar.com")),
      phoneNumber = Some(PhoneNumber("000000000"))
    )

  final val exampleContactAddress: ContactAddress =
    ContactAddress(
      line1 = "1 Foo Road",
      line2 = None,
      line3 = None,
      line4 = "Foobar",
      postcode = "FO1 1BR",
      country = Country.uk
    )

  final val exampleInspectionAddress: InspectionAddress =
    InspectionAddress(
      addressLine1 = Some("1 Bar Road"),
      addressLine2 = Some("Lewisham"),
      addressLine3 = None,
      city = Some("London"),
      postalCode = Some("BA1 1FO"),
      countryCode = Some(Country.uk.code),
      addressType = InspectionAddressType.Other
    )

  final val exampleInspectionDate: InspectionDate =
    InspectionDate(LocalDate.parse("2000-01-01"))

  final val exampleBankAccountDetails =
    BankAccountDetails(
      accountName = AccountName("Foo Bar"),
      sortCode = SortCode("00000000"),
      accountNumber = AccountNumber("00000000")
    )

  final val exampleDeclarantBankAccountDetails =
    BankAccountDetails(
      accountName = AccountName("Declarant"),
      sortCode = SortCode("000001"),
      accountNumber = AccountNumber("00000001")
    )

  final val exampleConsigneeBankAccountDetails =
    BankAccountDetails(
      accountName = AccountName("Consignee"),
      sortCode = SortCode("000002"),
      accountNumber = AccountNumber("00000002")
    )

  final val exampleRejectedGoodsDetails: String        = "Some example details for rejected goods"
  final val exampleSpecialCircumstancesDetails: String = "Goods failed health and safety inspection"

  final val exampleContactInformation1: ContactInformation =
    ContactInformation(
      contactPerson = Some("Foo Bar"),
      addressLine1 = Some("7 Flower Street"),
      addressLine2 = Some("Daisy House"),
      addressLine3 = None,
      street = Some("7 Flower Street"),
      city = Some("Bloomingham"),
      countryCode = Some("UK"),
      postalCode = Some("AA1AAB"),
      telephoneNumber = None,
      faxNumber = None,
      emailAddress = None
    )

  final val exampleContactInformation2: ContactInformation =
    ContactInformation(
      contactPerson = Some("Kind Man"),
      addressLine1 = Some("1 Barn Crescent"),
      addressLine2 = None,
      addressLine3 = None,
      street = Some("1 Barn Crescent"),
      city = Some("Loolipools"),
      countryCode = Some("UK"),
      postalCode = Some("AA2AAA"),
      telephoneNumber = None,
      faxNumber = None,
      emailAddress = None
    )

  final val exampleClaimantInformation =
    ClaimantInformation(
      eori = exampleEori,
      fullName = "",
      establishmentAddress = exampleContactInformation1,
      contactInformation = exampleContactInformation2
    )

  final val exampleSupportingEvidences =
    Seq(
      EvidenceDocument(
        checksum = "",
        downloadUrl = "",
        fileName = "",
        fileMimeType = "image/jpeg",
        size = 1234L,
        uploadedOn = LocalDateTime.now(),
        documentType = UploadDocumentType.CommercialInvoice
      ),
      EvidenceDocument(
        checksum = "",
        downloadUrl = "",
        fileName = "",
        fileMimeType = "application/pdf",
        size = 567L,
        uploadedOn = LocalDateTime.now(),
        documentType = UploadDocumentType.Other
      )
    )

  final val exampleScheduledDocument =
    EvidenceDocument(
      checksum = "",
      downloadUrl = "",
      fileName = "",
      fileMimeType = "image/png",
      size = 9876L,
      uploadedOn = LocalDateTime.now(),
      documentType = UploadDocumentType.ScheduleOfMRNs
    )

  final def buildDisplayDeclaration(
    id: String = exampleMrnAsString,
    declarantEORI: Eori = exampleEori,
    consigneeEORI: Option[Eori] = None,
    dutyDetails: Seq[(TaxCode, BigDecimal, Boolean)] = Seq.empty,
    consigneeContact: Option[ContactDetails] = None,
    declarantContact: Option[ContactDetails] = None,
    declarantBankDetails: Option[BankAccountDetails] = None,
    consigneeBankDetails: Option[BankAccountDetails] = None,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None
  ): DisplayDeclaration = {
    val ndrcDetails: List[NdrcDetails] =
      dutyDetails.zipWithIndex.map { case ((taxCode, paidAmount, cmaEligible), index) =>
        NdrcDetails(
          taxType = taxCode.value,
          amount = paidAmount.toString(),
          paymentMethod = generateSubsidyPayments match {
            case GenerateSubsidyPayments.None                  => if cmaEligible then "002" else "001"
            case GenerateSubsidyPayments.Some                  => if index % 2 == 1 then "006" else if cmaEligible then "002" else "001"
            case GenerateSubsidyPayments.All                   => "006"
            case GenerateSubsidyPayments.ForTaxCodes(taxCodes) =>
              if taxCodes.contains(taxCode) then "006" else if cmaEligible then "002" else "001"
          },
          paymentReference = s"payment-reference-$id",
          cmaEligible = if cmaEligible then Some("1") else None
        )
      }.toList

    val consigneeDetails: Option[ConsigneeDetails] =
      if consigneeEORI.contains(declarantEORI) then None
      else
        consigneeEORI.map(eori =>
          ConsigneeDetails(
            consigneeEORI = eori.value,
            legalName = s"consignee-legal-name-$id",
            establishmentAddress = EstablishmentAddress(
              addressLine1 = s"consignee-address-line-1-$id",
              countryCode = Country.uk.code
            ),
            contactDetails = consigneeContact
          )
        )

    val bankDetails: Option[BankDetails] = (declarantBankDetails, consigneeBankDetails) match {
      case (None, None) => None
      case (d, c)       => Some(BankDetails(c, d))
    }

    DisplayDeclaration {
      DisplayResponseDetail(
        declarationId = id,
        acceptanceDate = "11 October 2021",
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
            countryCode = Country.uk.code
          ),
          contactDetails = declarantContact
        ),
        consigneeDetails = consigneeDetails,
        accountDetails = None,
        bankDetails = bankDetails,
        maskedBankDetails = None,
        ndrcDetails = if ndrcDetails.isEmpty then None else Some(ndrcDetails)
      )
    }
  }

  final def buildSecuritiesDisplayDeclaration(
    id: String = exampleMrnAsString,
    securityReason: String,
    declarantEORI: Eori = exampleEori,
    consigneeEORI: Option[Eori] = None,
    depositDetails: Seq[(String, Seq[(TaxCode, BigDecimal)])] = Seq.empty,
    consigneeContact: Option[ContactDetails] = None,
    declarantContact: Option[ContactDetails] = None,
    allDutiesGuaranteeEligible: Boolean = false
  ): DisplayDeclaration = {
    val securityDetails: List[SecurityDetails] = depositDetails.map { case (securityDepositId, taxDetails) =>
      val totalAmount = taxDetails.map(_._2).sum
      SecurityDetails(
        securityDepositId = securityDepositId,
        totalAmount = totalAmount.toString(),
        amountPaid = totalAmount.toString(),
        paymentMethod = if allDutiesGuaranteeEligible then "004" else "001",
        paymentReference = s"payment-reference-$id",
        taxDetails = taxDetails.map { case (taxCode, amount) =>
          TaxDetails(taxCode.toString(), amount.toString())
        }.toList
      )
    }.toList

    val consigneeDetails: Option[ConsigneeDetails] =
      if consigneeEORI.contains(declarantEORI) then None
      else
        consigneeEORI.map(eori =>
          ConsigneeDetails(
            consigneeEORI = eori.value,
            legalName = s"consignee-legal-name-$id",
            establishmentAddress = EstablishmentAddress(
              addressLine1 = s"consignee-address-line-1-$id",
              countryCode = Country.uk.code
            ),
            contactDetails = consigneeContact
          )
        )

    DisplayDeclaration {
      DisplayResponseDetail(
        declarationId = id,
        acceptanceDate = "11 October 2021",
        declarantReferenceNumber = None,
        securityReason = Some(securityReason),
        btaDueDate = Some("2022-03-22"),
        procedureCode = "procedure-code",
        btaSource = None,
        declarantDetails = DeclarantDetails(
          declarantEORI = declarantEORI.value,
          legalName = s"declarant-legal-name-$id",
          establishmentAddress = EstablishmentAddress(
            addressLine1 = s"declarant-address-line-1-$id",
            countryCode = Country.uk.code
          ),
          contactDetails = declarantContact
        ),
        consigneeDetails = consigneeDetails,
        accountDetails = None,
        bankDetails = None,
        maskedBankDetails = None,
        ndrcDetails = None,
        securityDetails = if securityDetails.isEmpty then None else Some(securityDetails)
      )
    }
  }

  final def buildUploadDocument(id: String) = UploadedFile(
    upscanReference = s"upscan-reference-$id",
    fileName = s"file-name-$id",
    downloadUrl = s"download-url-$id",
    uploadTimestamp = ZonedDateTime.ofInstant(Instant.ofEpochMilli(0L), ZoneId.of("Europe/London")),
    checksum = "A" * 64,
    fileMimeType = s"application/$id",
    fileSize = Some(12345)
  )

  final val bigDecimalFormatter: Formatter[BigDecimal] = FormUtils
    .bigDecimalFormat(13, 2, "actual-amount.error.invalid")

  final def formatAmount(amount: BigDecimal): String =
    bigDecimalFormatter
      .unbind("key", amount)
      .get("key")
      .get

  final def nextTaxCode(seq: Seq[TaxCode], current: TaxCode): TaxCode =
    seq(seq.indexOf(current) + 1)

  sealed trait GenerateSubsidyPayments
  object GenerateSubsidyPayments {
    case object None extends GenerateSubsidyPayments
    case object Some extends GenerateSubsidyPayments
    case object All extends GenerateSubsidyPayments
    sealed case class ForTaxCodes(taxCodes: Set[TaxCode]) extends GenerateSubsidyPayments
  }

}
