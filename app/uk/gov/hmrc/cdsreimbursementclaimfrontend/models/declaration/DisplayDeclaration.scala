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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import cats.Eq
import cats.Id
import cats.implicits._
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.MissingAnswerError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.Validator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

final case class DisplayDeclaration(
  displayResponseDetail: DisplayResponseDetail
) {

  def getConsigneeDetails: Option[ConsigneeDetails] =
    displayResponseDetail.consigneeDetails

  def getDeclarantDetails: DeclarantDetails =
    displayResponseDetail.declarantDetails

  def getConsigneeEori: Option[Eori] =
    getConsigneeDetails.map(_.consigneeEORI).map(Eori.apply)

  def getDeclarantEori: Eori =
    Eori(getDeclarantDetails.declarantEORI)

  def getNdrcDetailsList: Option[List[NdrcDetails]] =
    displayResponseDetail.ndrcDetails

  def getNdrcDetailsFor(taxType: String): Option[NdrcDetails] =
    getNdrcDetailsList.flatMap(_.find(_.taxType === taxType))

  def getAvailableTaxCodes: Seq[TaxCode] =
    getNdrcDetailsList.map(_.map(d => TaxCode(d.taxType))).getOrElse(Seq.empty)

  def getMRN: MRN = MRN(displayResponseDetail.declarationId)

  def withDeclarationId(declarationId: String): DisplayDeclaration =
    copy(displayResponseDetail = displayResponseDetail.copy(declarationId = declarationId))

  def withBankDetails(bankDetails: Option[BankDetails]): DisplayDeclaration =
    copy(displayResponseDetail = displayResponseDetail.copy(bankDetails = bankDetails))

}

object DisplayDeclaration {

  val validator: Validator[Id, DisplayDeclaration] = maybeDisplayDeclaration =>
    maybeDisplayDeclaration.toValidNel(MissingAnswerError("Display declaration"))

  // TODO: not good code, most of this needed to be mapped when parsing from JSON
  // Same as devs must know about some workaround extension class import which not always the case
  implicit class DisplayDeclarationOps(val displayDeclaration: DisplayDeclaration) extends AnyVal {

    def totalPaidCharges: BigDecimal =
      BigDecimal(
        displayDeclaration.displayResponseDetail.ndrcDetails.fold(0.0)(ndrcDetails =>
          ndrcDetails.map(s => s.amount.toDouble).sum
        )
      )

    def consigneeName: Option[String] =
      displayDeclaration.displayResponseDetail.consigneeDetails.map(details => details.legalName)

    def consigneeEmail: Option[String] =
      displayDeclaration.displayResponseDetail.consigneeDetails.flatMap(details =>
        details.contactDetails.flatMap(f => f.emailAddress)
      )

    def consigneeTelephone: Option[String] =
      displayDeclaration.displayResponseDetail.consigneeDetails.flatMap(details =>
        details.contactDetails.flatMap(f => f.telephone)
      )

    def consigneeAddress: Option[String] =
      displayDeclaration.displayResponseDetail.consigneeDetails.map(details =>
        establishmentAddress(details.establishmentAddress).mkString("<br />")
      )

    def declarantName: String = displayDeclaration.displayResponseDetail.declarantDetails.legalName

    def declarantEmailAddress: Option[String] =
      displayDeclaration.displayResponseDetail.declarantDetails.contactDetails.flatMap(details => details.emailAddress)

    def declarantTelephoneNumber: Option[String] =
      displayDeclaration.displayResponseDetail.declarantDetails.contactDetails.flatMap(details => details.telephone)

    def declarantContactAddress: Option[String] =
      Option(displayDeclaration.displayResponseDetail.declarantDetails.establishmentAddress).map(address =>
        establishmentAddress(address).mkString("<br />")
      )

    def declarantAddress(contactDetails: ContactDetails): List[String] = {
      val lines = List(
        contactDetails.addressLine1,
        contactDetails.addressLine2,
        contactDetails.addressLine3,
        contactDetails.addressLine4,
        contactDetails.postalCode,
        contactDetails.countryCode
      )
      lines.collect { case Some(s) => s }
    }

    def establishmentAddress(establishmentAddress: EstablishmentAddress): List[String] =
      List(
        Some(establishmentAddress.addressLine1),
        establishmentAddress.addressLine2,
        establishmentAddress.addressLine3,
        establishmentAddress.postalCode,
        Some(establishmentAddress.countryCode)
      ).flattenOption
  }

  implicit val format: OFormat[DisplayDeclaration] = Json.format[DisplayDeclaration]

  implicit val equality: Eq[DisplayDeclaration] = Eq.fromUniversalEquals[DisplayDeclaration]
}
