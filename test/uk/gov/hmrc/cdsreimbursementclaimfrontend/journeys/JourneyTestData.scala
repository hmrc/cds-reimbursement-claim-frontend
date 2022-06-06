/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.syntax.eq._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneId
import java.time.ZonedDateTime
import play.api.data.format.Formatter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils
import java.lang

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
          if (error === expectedError) ()
          else
            throw new Exception(
              s"Journey construction in ${pos.fileName}:${pos.lineNumber} has failed as expected, but error was different: $error"
            ),
        _ =>
          throw new Exception(
            s"Expected failure but journey construction succeeded in ${pos.fileName}:${pos.lineNumber}"
          )
      )
  }

  implicit class SeqOps[A](val seq: Seq[A]) {
    def halfNonEmpty: Seq[A] =
      if (seq.isEmpty) throw new lang.Error("Cannot shrink the sequence because is empty.")
      else if (seq.size > 1) seq.take(seq.size / 2)
      else seq

    def secondHalfNonEmpty: Seq[A] =
      if (seq.isEmpty) throw new lang.Error("Cannot shrink the sequence because is empty.")
      else if (seq.size > 1) seq.drop(seq.length - seq.size / 2)
      else seq

    def headSeq: Seq[A] =
      seq.headOption.map(Seq(_)).getOrElse(Seq.empty)

    def takeExceptIn(other: Seq[A]): Seq[A] =
      seq.filterNot(other.contains(_))
  }

  final val exampleEori: Eori           = IdGen.genEori.sample.get
  final val anotherExampleEori: Eori    = IdGen.genEori.sample.get
  final val yetAnotherExampleEori: Eori = IdGen.genEori.sample.get

  final val exampleEoriAsString: String = exampleEori.value

  final val exampleMrn: MRN            = IdGen.genMRN.sample.get
  final val anotherExampleMrn: MRN     = IdGen.genMRN.sample.get
  final val exampleMrnAsString: String = exampleMrn.value

  final val uploadDocument = buildUploadDocument("foo")

  final val exampleContactDetails: MrnContactDetails =
    MrnContactDetails(
      fullName = "Foo Bar",
      emailAddress = Email("foo@bar.com"),
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

  final val exampleRejectedGoodsDetails: String        = "Some example details for rejected goods"
  final val exampleSpecialCircumstancesDetails: String = "Goods failed health and safety inspection"

  final def buildDisplayDeclaration(
    id: String = exampleMrnAsString,
    declarantEORI: Eori = exampleEori,
    consigneeEORI: Option[Eori] = None,
    dutyDetails: Seq[(TaxCode, BigDecimal, Boolean)] = Seq.empty,
    consigneeContact: Option[ContactDetails] = None,
    declarantContact: Option[ContactDetails] = None
  ): DisplayDeclaration = {
    val ndrcDetails: List[NdrcDetails] =
      dutyDetails.map { case (taxCode, paidAmount, cmaEligible) =>
        NdrcDetails(
          taxType = taxCode.value,
          amount = paidAmount.toString(),
          paymentMethod = if (cmaEligible) "002" else "001",
          paymentReference = s"payment-reference-$id",
          cmaEligible = if (cmaEligible) Some("1") else None
        )
      }.toList

    val consigneeDetails: Option[ConsigneeDetails] =
      if (consigneeEORI.contains(declarantEORI))
        None
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
            countryCode = Country.uk.code
          ),
          contactDetails = declarantContact
        ),
        consigneeDetails = consigneeDetails,
        accountDetails = None,
        bankDetails = None,
        maskedBankDetails = None,
        ndrcDetails = if (ndrcDetails.isEmpty) None else Some(ndrcDetails)
      )
    }
  }

  final def buildSecuritiesDisplayDeclaration(
    id: String = exampleMrnAsString,
    declarantEORI: Eori = exampleEori,
    consigneeEORI: Option[Eori] = None,
    reclaimsDetails: Seq[(String, Seq[(TaxCode, BigDecimal)])] = Seq.empty,
    consigneeContact: Option[ContactDetails] = None,
    declarantContact: Option[ContactDetails] = None,
    guaranteeEligible: Boolean = false
  ): DisplayDeclaration = {
    val securityDetails: List[SecurityDetails] = reclaimsDetails.map { case (securityDepositId, taxDetails) =>
      val totalAmount = taxDetails.map(_._2).sum
      SecurityDetails(
        securityDepositId = securityDepositId,
        totalAmount = totalAmount.toString(),
        amountPaid = totalAmount.toString(),
        paymentMethod = if (guaranteeEligible) "004" else "003",
        paymentReference = s"payment-reference-$id",
        taxDetails = taxDetails.map { case (taxCode, amount) =>
          TaxDetails(taxCode.toString(), amount.toString())
        }.toList
      )
    }.toList

    val consigneeDetails: Option[ConsigneeDetails] =
      if (consigneeEORI.contains(declarantEORI))
        None
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
            countryCode = Country.uk.code
          ),
          contactDetails = declarantContact
        ),
        consigneeDetails = consigneeDetails,
        accountDetails = None,
        bankDetails = None,
        maskedBankDetails = None,
        ndrcDetails = None,
        securityDetails = if (securityDetails.isEmpty) None else Some(securityDetails)
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

}
