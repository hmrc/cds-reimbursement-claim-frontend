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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import cats.implicits._
import cats.Eq
import cats.Id
import play.api.i18n.Messages
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.MissingAnswerError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.Validator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes

final case class DisplayDeclaration(
  displayResponseDetail: DisplayResponseDetail
) {

  def hasBankDetails: Boolean                       =
    displayResponseDetail.bankDetails.isDefined
  def getConsigneeDetails: Option[ConsigneeDetails] =
    displayResponseDetail.consigneeDetails

  def getDeclarantDetails: DeclarantDetails =
    displayResponseDetail.declarantDetails

  def getConsigneeEori: Option[Eori] =
    getConsigneeDetails.map(_.consigneeEORI).map(Eori.apply)

  def getDeclarantEori: Eori =
    Eori(getDeclarantDetails.declarantEORI)

  def containsXiEori: Boolean =
    getDeclarantEori.isXiEori || getConsigneeEori.exists(_.isXiEori)

  def getNdrcDetailsList: Option[List[NdrcDetails]] =
    displayResponseDetail.ndrcDetails

  def getNdrcDetailsFor(taxType: String): Option[NdrcDetails] =
    getNdrcDetailsList.flatMap(_.find(_.taxType === taxType))

  lazy val getNdrcDutiesWithAmount: Option[List[(TaxCode, BigDecimal)]] =
    getNdrcDetailsList
      .map(_.map(ndrcDetails => (TaxCode(ndrcDetails.taxType), BigDecimal(ndrcDetails.amount))))

  def getAvailableTaxCodes: Seq[TaxCode] =
    getNdrcDetailsList.map(_.map(d => TaxCode(d.taxType))).getOrElse(Seq.empty)

  def getMRN: MRN = MRN(displayResponseDetail.declarationId)

  def getReasonForSecurity: Option[ReasonForSecurity] =
    displayResponseDetail.securityReason.flatMap(ReasonForSecurity.fromACC14Code)

  def getSecurityDepositIds: Option[List[String]] =
    displayResponseDetail.securityDetails.map(_.map(_.securityDepositId))

  def getNumberOfSecurityDeposits: Int = getSecurityDepositIds
    .map(_.size)
    .getOrElse(0)

  def getSecurityDepositIdIndex(securityDepositId: String): Int =
    getSecurityDepositIds
      .map(_.indexOf(securityDepositId))
      .getOrElse(-1)

  def isValidSecurityDepositId(securityDepositId: String): Boolean =
    displayResponseDetail.securityDetails
      .exists(_.exists(_.securityDepositId === securityDepositId))

  def getSecurityTaxCodesFor(securityDepositId: String): List[TaxCode] =
    getSecurityDetailsFor(securityDepositId)
      .map(
        _.taxDetails.map(_.getTaxCode).distinct
      )
      .getOrElse(Nil)

  def getSecurityDetailsFor(securityDepositId: String): Option[SecurityDetails] =
    displayResponseDetail.securityDetails
      .getOrElse(Nil)
      .find(_.securityDepositId === securityDepositId)

  def getAllSecurityDetailsFor(securityDepositId: String): Seq[SecurityDetails] =
    displayResponseDetail.securityDetails
      .getOrElse(Nil)
      .filter(_.securityDepositId === securityDepositId)

  def getSecurityTaxDetailsFor(securityDepositId: String, taxCode: TaxCode): Option[TaxDetails] =
    getSecurityDetailsFor(securityDepositId)
      .flatMap(_.taxDetails.find(td => td.getTaxCode === taxCode))

  def getSecurityTotalValueFor(securityDepositId: String): BigDecimal =
    getSecurityDetailsFor(securityDepositId)
      .map(_.getTotalAmount)
      .getOrElse(BigDecimal("0.00"))

  def getSecurityPaidValueFor(securityDepositId: String): BigDecimal =
    getSecurityDetailsFor(securityDepositId)
      .map(_.getPaidAmount)
      .getOrElse(BigDecimal("0.00"))

  def getSecurityPaymentReferenceFor(securityDepositId: String): String =
    getSecurityDetailsFor(securityDepositId).map(_.paymentReference).getOrElse("")

  def getTotalSecuritiesAmountFor(securityDepositIds: Set[String]): BigDecimal =
    securityDepositIds
      .map(getSecurityDetailsFor(_).map(_.getTotalAmount).getOrElse(BigDecimal("0.00")))
      .sum

  def getTotalSecuritiesPaidAmountFor(securityDepositIds: Set[String]): BigDecimal =
    securityDepositIds
      .map(getSecurityDetailsFor(_).map(_.getPaidAmount).getOrElse(BigDecimal("0.00")))
      .sum

  def withDeclarationId(declarationId: String): DisplayDeclaration =
    copy(displayResponseDetail = displayResponseDetail.copy(declarationId = declarationId))

  def optionallyWithMRN(maybeMRN: Option[MRN]): DisplayDeclaration =
    maybeMRN
      .map(mrn => copy(displayResponseDetail = displayResponseDetail.copy(declarationId = mrn.value)))
      .getOrElse(this)

  def withDeclarantEori(eori: Eori): DisplayDeclaration =
    copy(displayResponseDetail =
      displayResponseDetail.copy(declarantDetails =
        displayResponseDetail.declarantDetails.copy(declarantEORI = eori.value)
      )
    )

  def withConsigneeEori(eori: Eori): DisplayDeclaration =
    copy(displayResponseDetail =
      displayResponseDetail.copy(consigneeDetails =
        displayResponseDetail.consigneeDetails.map(_.copy(consigneeEORI = eori.value))
      )
    )

  def withBankDetails(bankDetails: Option[BankDetails]): DisplayDeclaration =
    copy(displayResponseDetail = displayResponseDetail.copy(bankDetails = bankDetails))

  def withReasonForSecurity(reasonForSecurity: ReasonForSecurity): DisplayDeclaration =
    copy(displayResponseDetail = displayResponseDetail.copy(securityReason = Some(reasonForSecurity.acc14Code)))

  def hasSameEoriAs(other: DisplayDeclaration): Boolean =
    this.getDeclarantEori === other.getDeclarantEori ||
      this.getConsigneeEori.exists(eori => other.getConsigneeEori.contains(eori))

  def isFullSecurityAmount(securityDepositId: String, reclaimedAmount: BigDecimal): Boolean =
    getSecurityDetailsFor(securityDepositId)
      .exists(_.getTotalAmount === reclaimedAmount)

  def isAllSelectedSecuritiesEligibleForBankAccount(securityDepositIds: Set[String]): Boolean = {
    val eligible: Set[Boolean] = securityDepositIds
      .map(sid => getAllSecurityDetailsFor(sid).exists(_.isBankAccountPayment))

    eligible.nonEmpty && eligible.forall(identity)
  }

  def isAllSelectedSecuritiesEligibleForCashAccount(securityDepositIds: Set[String]): Boolean = {
    val eligible: Set[Boolean] = securityDepositIds
      .map(sid => getAllSecurityDetailsFor(sid).exists(_.isCashAccount))

    eligible.nonEmpty && eligible.forall(identity)
  }

  def isAllSelectedSecuritiesEligibleForDefermentAccount(securityDepositIds: Set[String]): Boolean = {
    val eligible: Set[Boolean] = securityDepositIds
      .map(sid => getAllSecurityDetailsFor(sid).exists(_.isDefermentAccount))

    eligible.nonEmpty && eligible.forall(identity)
  }

  def isAllSelectedSecuritiesEligibleForDifferentRepaymentMethods(securityDepositIds: Set[String]): Boolean =
    securityDepositIds
      .flatMap(getAllSecurityDetailsFor(_).map(_.paymentMethod))
      .size > 1

  def isAllSelectedSecuritiesEligibleForGuaranteePayment(securityDepositIds: Set[String]): Boolean = {
    val eligible: Set[Boolean] = securityDepositIds
      .map(sid => getSecurityDetailsFor(sid).exists(_.isGuaranteeEligible))

    eligible.nonEmpty && eligible.forall(identity)
  }

  def isAnySelectedSecuritiesEligibleForBankAccountPayment(securityDepositIds: Set[String]): Boolean =
    securityDepositIds
      .exists(sid => getSecurityDetailsFor(sid).exists(_.isBankAccountPayment))

  def hasSomeSubsidyPayment: Boolean =
    displayResponseDetail.ndrcDetails.exists(_.exists(_.hasSubsidyPaymentMethod))

  def hasOnlySubsidyPayments: Boolean =
    displayResponseDetail.ndrcDetails.exists(_.forall(_.hasSubsidyPaymentMethod))

}

object DisplayDeclaration {

  val validator: Validator[Id, DisplayDeclaration] = maybeDisplayDeclaration =>
    maybeDisplayDeclaration.toValidNel(MissingAnswerError("Display declaration"))

  // TODO: not good code, most of this needed to be mapped when parsing from JSON
  // Same as devs must know about some workaround extension class import which not always the case
  implicit class DisplayDeclarationOps(private val displayDeclaration: DisplayDeclaration) extends AnyVal {

    def totalVatPaidCharges: BigDecimal =
      BigDecimal(
        displayDeclaration.displayResponseDetail.ndrcDetails
          .map(
            _.filter(ndrc => TaxCodes.vatTaxCodes.contains(TaxCode(ndrc.taxType)))
          )
          .fold(0.0)(ndrcDetails => ndrcDetails.map(s => s.amount.toDouble).sum)
      )

    def totalDutiesPaidCharges: BigDecimal =
      BigDecimal(
        displayDeclaration.displayResponseDetail.ndrcDetails
          .map(
            _.filterNot(ndrc => TaxCodes.vatTaxCodes.contains(TaxCode(ndrc.taxType)))
          )
          .fold(0.0)(ndrcDetails => ndrcDetails.map(s => s.amount.toDouble).sum)
      )

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
        details.contactDetails.flatMap(f => f.maybeEmailAddress)
      )

    def consigneeTelephone: Option[String] =
      displayDeclaration.displayResponseDetail.consigneeDetails.flatMap(details =>
        details.contactDetails.flatMap(f => f.telephone)
      )

    def consigneeAddress(implicit messages: Messages): Option[String] =
      displayDeclaration.displayResponseDetail.consigneeDetails.map(details =>
        establishmentAddress(details.establishmentAddress).mkString("<br />")
      )

    def declarantName: String = displayDeclaration.displayResponseDetail.declarantDetails.legalName

    def declarantEmailAddress: Option[String] =
      displayDeclaration.displayResponseDetail.declarantDetails.contactDetails.flatMap(details =>
        details.maybeEmailAddress
      )

    def declarantTelephoneNumber: Option[String] =
      displayDeclaration.displayResponseDetail.declarantDetails.contactDetails.flatMap(details => details.telephone)

    def declarantContactAddress(implicit messages: Messages): Option[String] =
      Option(displayDeclaration.displayResponseDetail.declarantDetails.establishmentAddress).map(address =>
        establishmentAddress(address).mkString("<br />")
      )

    def declarantAddress(contactDetails: ContactDetails)(implicit messages: Messages): List[String] = {
      val lines = List(
        contactDetails.addressLine1,
        contactDetails.addressLine2,
        contactDetails.addressLine3,
        contactDetails.addressLine4,
        contactDetails.postalCode,
        contactDetails.countryCode
          .map(countryCode => messages(s"country.$countryCode"))
      )
      lines.collect { case Some(s) => s }
    }

    def establishmentAddress(establishmentAddress: EstablishmentAddress)(implicit messages: Messages): List[String] =
      List(
        Some(establishmentAddress.addressLine1),
        establishmentAddress.addressLine2,
        establishmentAddress.addressLine3,
        establishmentAddress.postalCode,
        Some(establishmentAddress.countryCode)
          .map(countryCode => messages(s"country.$countryCode"))
      ).flattenOption
  }

  implicit val format: OFormat[DisplayDeclaration] = Json.format[DisplayDeclaration]

  implicit val equality: Eq[DisplayDeclaration] = Eq.fromUniversalEquals[DisplayDeclaration]
}
