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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import play.api.libs.json.{Json, OFormat}

final case class Declaration(
  declarantId: String,
  acceptanceDate: String,
  declarantDetails: DeclarantDetails,
  consigneeDetails: Option[ConsigneeDetails],
  maskedBankDetails: Option[MaskedBankDetails],
  securityDetails: Option[List[SecurityDetails]]
)

object Declaration {

  implicit class DeclarationOps(declaration: Declaration) {

    //TODO: make util to handle financial values
    def totalPaidCharges: Double =
      declaration.securityDetails.fold(0.0)(securityDetails => securityDetails.map(s => s.totalAmount.toDouble).sum)

    def declarantEmailAddress: String = declaration.declarantDetails.contactDetails match {
      case Some(value) => value.emailAddress.getOrElse("no email address")
      case None        => "no contact details"
    }

    def declarantTelephoneNumber: String = declaration.declarantDetails.contactDetails match {
      case Some(value) => value.telephone.getOrElse("no telephone address")
      case None        => "mp contact details"
    }

    def declarantContactAddress: String = declaration.declarantDetails.contactDetails match {
      case Some(value) =>
        s"${value.addressLine1.getOrElse("")}, ${value.addressLine2.getOrElse("no line 1")}, ${value.addressLine3
          .getOrElse("no line 2")}, ${value.addressLine4
          .getOrElse("")}, ${value.postalCode.getOrElse("")}, ${value.countryCode.getOrElse("")}"
      case None        => "mp contact details"
    }

    def companyName: String = declaration.consigneeDetails match {
      case Some(value) => value.legalName
      case None        => "could mot get name"
    }

    def companyAddress: String = declaration.consigneeDetails match {
      case Some(value) =>
        s"${value.establishmentAddress.addressLine1} ${value.establishmentAddress.addressLine2.getOrElse("")} ${value.establishmentAddress.addressLine3
          .getOrElse("")} ${value.establishmentAddress.postalCode.getOrElse("")} ${value.establishmentAddress.countryCode}"
      case None        => "could mot get name"
    }

  }

  implicit val format: OFormat[Declaration] = Json.format[Declaration]
}
