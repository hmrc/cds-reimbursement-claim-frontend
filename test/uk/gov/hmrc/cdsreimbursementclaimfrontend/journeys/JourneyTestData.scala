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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

trait JourneyTestData {

  implicit class EitherOps[A](val either: Either[String, A]) {
    def getOrFail(implicit pos: org.scalactic.source.Position): A =
      either.fold(
        error =>
          throw new Exception(
            s"Journey construction in ${pos.fileName}:${pos.lineNumber} has failed because of $error"
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

  val exampleEori: Eori           = IdGen.genEori.sample.get
  val anotherExampleEori: Eori    = IdGen.genEori.sample.get
  val yetAnotherExampleEori: Eori = IdGen.genEori.sample.get

  val exampleEoriAsString: String = exampleEori.value

  val exampleMrn: MRN            = IdGen.genMRN.sample.get
  val exampleMrnAsString: String = exampleMrn.value

  val uploadDocument = buildUploadDocument("foo")

  val exampleContactDetails: MrnContactDetails =
    MrnContactDetails(
      fullName = "Foo Bar",
      emailAddress = Email("foo@bar.com"),
      phoneNumber = Some(PhoneNumber("000000000"))
    )

  val exampleContactAddress: ContactAddress =
    ContactAddress(
      line1 = "1 Foo Road",
      line2 = None,
      line3 = None,
      line4 = "Foobar",
      postcode = "FO1 1BR",
      country = Country.uk
    )

  val exampleInspectionAddress: InspectionAddress =
    InspectionAddress(
      addressLine1 = "1 Bar Road",
      addressLine2 = "Lewisham",
      city = "London",
      postalCode = "BA1 1FO",
      countryCode = Country.uk.code,
      addressType = InspectionAddressType.Other
    )

  val exampleInspectionDate: InspectionDate =
    InspectionDate(LocalDate.parse("2000-01-01"))

  val exampleBankAccountDetails =
    BankAccountDetails(
      accountName = AccountName("Foo Bar"),
      sortCode = SortCode("00000000"),
      accountNumber = AccountNumber("00000000")
    )

  val exampleRejectedGoodsDetails: String        = "Some example details for rejected goods"
  val exampleSpecialCircumstancesDetails: String = "Goods failed health and safety inspection"

  def buildDisplayDeclaration(
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
          paymentMethod = s"payment-method-$id",
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
              countryCode = s"consignee-country-code-$id"
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
            countryCode = s"declarant-country-code-$id"
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

  def buildUploadDocument(id: String) = UploadedFile(
    upscanReference = s"upscan-reference-$id",
    fileName = s"file-name-$id",
    downloadUrl = s"download-url-$id",
    uploadTimestamp = ZonedDateTime.ofInstant(Instant.ofEpochMilli(0L), ZoneId.of("Europe/London")),
    checksum = "A" * 64,
    fileMimeType = s"application/$id",
    fileSize = Some(12345)
  )

}
