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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import play.api.libs.json.{Json, OFormat}

final case class GetDeclarationResponse(
  overpaymentDeclarationDisplayResponse: OverpaymentDeclarationDisplayResponse
)

object GetDeclarationResponse {
  implicit val returnParametersReads: OFormat[ReturnParameters]                                            = Json.format
  implicit val establishmentAddressReads: OFormat[EstablishmentAddress]                                    = Json.format
  implicit val contactDetailsReads: OFormat[ContactDetails]                                                = Json.format
  implicit val declarantDetailsReads: OFormat[DeclarantDetails]                                            = Json.format
  implicit val consigneeBankDetailsReads: OFormat[ConsigneeBankDetails]                                    = Json.format
  implicit val declarantBankDetailsReads: OFormat[DeclarantBankDetails]                                    = Json.format
  implicit val bankDetailsReads: OFormat[BankDetails]                                                      = Json.format
  implicit val taxDetailsReads: OFormat[TaxDetails]                                                        = Json.format
  implicit val consigneeDetailsReads: OFormat[ConsigneeDetails]                                            = Json.format
  implicit val accountDetailsReads: OFormat[AccountDetails]                                                = Json.format
  implicit val securityDetailsReads: OFormat[SecurityDetails]                                              = Json.format
  implicit val ndrcDetailsReads: OFormat[NdrcDetails]                                                      = Json.format
  implicit val responseDetailReads: OFormat[ResponseDetail]                                                = Json.format
  implicit val responseCommonReads: OFormat[ResponseCommon]                                                = Json.format
  implicit val overpaymentDeclarationDisplayResponseReader: OFormat[OverpaymentDeclarationDisplayResponse] = Json.format
  implicit val declarationInfoResponseReader: OFormat[GetDeclarationResponse]                              = Json.format
}

final case class OverpaymentDeclarationDisplayResponse(
  responseCommon: ResponseCommon,
  responseDetail: Option[ResponseDetail]
)

final case class ResponseCommon(
  status: String,
  statusText: Option[String],
  processingDate: String,
  returnParameters: Option[List[ReturnParameters]]
)

final case class ReturnParameters(
  paramName: String,
  paramValue: String
)

final case class ResponseDetail(
  declarationId: String,
  acceptanceDate: String,
  declarantReferenceNumber: Option[String],
  securityReason: Option[String],
  btaDueDate: Option[String],
  procedureCode: String,
  btaSource: Option[String],
  declarantDetails: DeclarantDetails,
  consigneeDetails: Option[ConsigneeDetails],
  accountDetails: Option[List[AccountDetails]],
  bankDetails: Option[BankDetails],
  securityDetails: Option[List[SecurityDetails]],
  ndrcDetails: Option[List[NdrcDetails]]
)

final case class DeclarantDetails(
  declarantEORI: String,
  legalName: String,
  establishmentAddress: EstablishmentAddress,
  contactDetails: Option[ContactDetails]
)

final case class EstablishmentAddress(
  addressLine1: String,
  addressLine2: Option[String],
  addressLine3: Option[String],
  postalCode: Option[String],
  countryCode: String
)

final case class ContactDetails(
  contactName: Option[String],
  addressLine1: Option[String],
  addressLine2: Option[String],
  addressLine3: Option[String],
  addressLine4: Option[String],
  postalCode: Option[String],
  countryCode: Option[String],
  telephone: Option[String],
  emailAddress: Option[String]
)

final case class ConsigneeDetails(
  consigneeEORI: String,
  legalName: String,
  establishmentAddress: EstablishmentAddress,
  contactDetails: Option[ContactDetails]
)

final case class AccountDetails(
  accountType: String,
  accountNumber: String,
  eori: String,
  legalName: String,
  contactDetails: Option[ContactDetails]
)

final case class BankDetails(
  consigneeBankDetails: Option[ConsigneeBankDetails],
  declarantBankDetails: Option[DeclarantBankDetails]
)

final case class ConsigneeBankDetails(
  accountHolderName: String,
  sortCode: String,
  accountNumber: String
)

final case class DeclarantBankDetails(
  accountHolderName: String,
  sortCode: String,
  accountNumber: String
)

final case class SecurityDetails(
  securityDepositId: String,
  totalAmount: String,
  amountPaid: String,
  paymentMethod: String,
  paymentReference: String,
  taxDetails: List[TaxDetails]
)

final case class TaxDetails(
  taxType: String,
  amount: String
)

final case class NdrcDetails(
  taxType: String,
  amount: String,
  paymentMethod: String,
  paymentReference: String,
  cmaEligible: Option[String]
)
