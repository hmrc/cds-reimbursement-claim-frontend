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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim

import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.util.UUID

import play.api.libs.json.{Json, OFormat}

// These classes represent TPI05 API requests details
object SubmitClaimRequest {

  implicit val establishmentAddressFormat: OFormat[EstablishmentAddress]       = Json.format
  implicit val cdsEstablishmentAddressFormat: OFormat[CdsEstablishmentAddress] = Json.format
  implicit val contactInformationFormat: OFormat[ContactInformation]           = Json.format
  implicit val vatDetailsFormat: OFormat[VatDetails]                           = Json.format
  implicit val agentEoriDetailsFormat: OFormat[AgentEoriDetails]               = Json.format
  implicit val goodsDetailsFormat: OFormat[GoodsDetails]                       = Json.format
  implicit val importerEoriDetailsFormat: OFormat[ImporterEoriDetails]         = Json.format
  implicit val eoriDetailsFormat: OFormat[EoriDetails]                         = Json.format
  implicit val contactDetailsFormat: OFormat[ContactDetails]                   = Json.format
  implicit val declarantDetailsFormat: OFormat[DeclarantDetails]               = Json.format
  implicit val accountDetailsFormat: OFormat[AccountDetails]                   = Json.format
  implicit val bankDetailsFormat: OFormat[BankDetails]                         = Json.format
  implicit val bankInfoFormat: OFormat[BankInfo]                               = Json.format
  implicit val ndrcDetailsFormat: OFormat[NdrcDetails]                         = Json.format
  implicit val mrnDetailsFormat: OFormat[MrnDetails]                           = Json.format
  implicit val entryDetailsFormat: OFormat[EntryDetails]                       = Json.format
  implicit val requestDetailFormat: OFormat[TPI05RequestDetail]                = Json.format
  val dateFormatter: DateTimeFormatter                                         =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'").withZone(ZoneId.systemDefault())

  def generateId: String = UUID.randomUUID().toString.replaceAll("-", "").take(31)

  final case class TPI05RequestDetail(
    CDFPayService: Option[String],
    dateReceived: Option[String],
    claimType: Option[String],
    caseType: Option[String],
    customDeclarationType: Option[String],
    declarationMode: Option[String],
    claimDate: Option[String],
    claimAmountTotal: Option[String],
    disposalMethod: Option[String],
    reimbursementMethod: Option[String],
    basisOfClaim: Option[String],
    claimant: Option[String],
    payeeIndicator: Option[String],
    newEORI: Option[String],
    newDAN: Option[String],
    authorityTypeProvided: Option[String],
    claimantEORI: Option[String],
    claimantEmailAddress: Option[String],
    goodsDetails: Option[GoodsDetails],
    EORIDetails: Option[EoriDetails],
    MRNDetails: Option[List[MrnDetails]],
    //DuplicateMRNDetails: Option[MrnDetails],
    entryDetails: Option[List[EntryDetails]]
    //duplicateEntryDetails: Option[EntryDetails]
  )

  final case class GoodsDetails(
    placeOfImport: Option[String],
    isPrivateImporter: Option[String],
    groundsForRepaymentApplication: Option[String],
    descOfGoods: Option[String]
  )

  final case class EoriDetails(
    agentEORIDetails: AgentEoriDetails,
    importerEORIDetails: ImporterEoriDetails
  )

  final case class AgentEoriDetails(
    EORINumber: String,
    CDSFullName: Option[String],
    legalEntityType: Option[String],
    EORIStartDate: Option[String],
    EORIEndDate: Option[String],
    CDSEstablishmentAddress: CdsEstablishmentAddress,
    contactInformation: Option[ContactInformation],
    VATDetails: Option[List[VatDetails]]
  )

  final case class CdsEstablishmentAddress(
    contactPerson: Option[String],
    addressline1: Option[String],
    addressline2: Option[String],
    addressline3: Option[String],
    street: Option[String],
    city: Option[String],
    countryCode: Option[String],
    postalCode: Option[String],
    telephone: Option[String],
    emailAddress: Option[String]
  )

  final case class ContactInformation(
    contactPerson: Option[String],
    addressline1: Option[String],
    addressline2: Option[String],
    addressline3: Option[String],
    street: Option[String],
    city: Option[String],
    countryCode: Option[String],
    postalCode: Option[String],
    telephoneNumber: Option[String],
    faxNumber: Option[String],
    emailAddress: Option[String]
  )

  final case class VatDetails(
    VATID: String,
    countryCode: String
  )

  final case class ImporterEoriDetails(
    EORINumber: String,
    CDSFullName: Option[String],
    legalEntityType: Option[String],
    EORIStartDate: Option[String],
    EORIEndDate: Option[String],
    CDSEstablishmentAddress: CdsEstablishmentAddress,
    contactInformation: Option[ContactInformation],
    VATDetails: Option[List[VatDetails]]
  )

  final case class MrnDetails(
    MRNNumber: Option[String],
    acceptanceDate: Option[String],
    declarantReferenceNumber: Option[String],
    mainDeclarationReference: Option[Boolean],
    procedureCode: Option[String],
    declarantDetails: Option[DeclarantDetails],
    accountDetails: Option[List[AccountDetails]],
    consigneeDetails: Option[DeclarantDetails],
    bankInfo: Option[BankInfo],
    NDRCDetails: Option[List[NdrcDetails]]
  )

  final case class EntryDetails(
    entryNumber: Option[String],
    entryDate: Option[String],
    declarantReferenceNumber: Option[String],
    mainDeclarationReference: Option[Boolean],
    declarantDetails: Option[DeclarantDetails],
    accountDetails: Option[List[AccountDetails]],
    consigneeDetails: Option[DeclarantDetails],
    bankInfo: Option[BankInfo],
    NDRCDetails: Option[List[NdrcDetails]]
  )

  final case class DeclarantDetails(
    EORI: String,
    legalName: String,
    establishmentAddress: EstablishmentAddress,
    contactDetails: ContactDetails
  )

  final case class EstablishmentAddress(
    contactPerson: Option[String],
    addressline1: Option[String],
    addressline2: Option[String],
    addressline3: Option[String],
    street: Option[String],
    city: Option[String],
    countryCode: Option[String],
    postalCode: Option[String],
    telephone: Option[String],
    emailAddress: Option[String]
  )

  final case class ContactDetails(
    contactPerson: Option[String],
    addressline1: Option[String],
    addressline2: Option[String],
    addressline3: Option[String],
    street: Option[String],
    city: Option[String],
    countryCode: Option[String],
    postalCode: Option[String],
    telephoneNumber: Option[String],
    faxNumber: Option[String],
    emailAddress: Option[String]
  )

  final case class AccountDetails(
    accountType: String,
    accountNumber: String,
    EORI: String,
    legalName: String,
    contactDetails: Option[ContactDetails]
  )

  final case class BankInfo(
    consigneeBankDetails: Option[BankDetails],
    declarantBankDetails: Option[BankDetails]
  )

  final case class BankDetails(
    accountHolderName: String,
    sortCode: String,
    accountNumber: String
  )

  final case class NdrcDetails(
    paymentMethod: String,
    paymentReference: String,
    CMAEligible: Option[String],
    taxType: String,
    amount: String,
    claimAmount: Option[String]
  )

  object MrnDetails {
    def apply(declarationId: String): MrnDetails =
      MrnDetails(
        MRNNumber = Some(declarationId),
        acceptanceDate = None,
        declarantReferenceNumber = None,
        mainDeclarationReference = Some(true),
        procedureCode = None,
        declarantDetails = None,
        accountDetails = None,
        consigneeDetails = None,
        bankInfo = None,
        NDRCDetails = None
      )
  }

  object EntryDetails {
    def apply(declarationId: String): EntryDetails =
      EntryDetails(
        entryNumber = Some(declarationId),
        entryDate = None,
        declarantReferenceNumber = None,
        mainDeclarationReference = Some(true),
        declarantDetails = None,
        accountDetails = None,
        consigneeDetails = None,
        bankInfo = None,
        NDRCDetails = None
      )
  }

}
