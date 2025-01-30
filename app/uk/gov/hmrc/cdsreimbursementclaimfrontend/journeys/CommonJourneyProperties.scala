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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementWithCorrectAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType.Declarant
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType.Representative

import java.time.LocalDateTime
import scala.collection.immutable.SortedMap
import java.time.Instant

/** Common properties and computations of all of the journeys. */
trait CommonJourneyProperties {

  def answers: CommonAnswers

  /** Case number is the final result of successfully submitting the claim. */
  def caseNumber: Option[String]
  def submissionDateTime: Option[LocalDateTime]
  def needsDocumentType: Boolean = true
  def startTimeSeconds: Long

  def declarantEoriMatchesConsignee: Boolean =
    getDeclarantEoriFromACC14.isDefined &&
      getDeclarantEoriFromACC14 === getConsigneeEoriFromACC14
  def getLeadMovementReferenceNumber: Option[MRN]
  def getLeadDisplayDeclaration: Option[DisplayDeclaration]
  def needsBanksAccountDetailsSubmission: Boolean
  def getDocumentTypesIfRequired: Option[Seq[UploadDocumentType]]

  def getDisplayDeclarations: Seq[DisplayDeclaration] =
    getLeadDisplayDeclaration.fold(Seq.empty[DisplayDeclaration])(Seq.apply(_))

  def validateDeclarationCandidate(declaration: DisplayDeclaration): Option[String] =
    None

  final val ZERO: BigDecimal = BigDecimal("0")

  final def hasCompleteSupportingEvidences: Boolean =
    answers.supportingEvidences.nonEmpty &&
      (!needsDocumentType ||
        answers.supportingEvidences.forall(_.documentType.isDefined))

  final def getConsigneeEoriFromACC14: Option[Eori] =
    getLeadDisplayDeclaration.flatMap(_.getConsigneeEori)

  final def getConsigneeContactDetailsFromACC14: Option[ContactDetails] =
    getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails).flatMap(_.contactDetails)

  final def getDeclarantEoriFromACC14: Option[Eori] =
    getLeadDisplayDeclaration.map(_.getDeclarantEori)

  final def getDeclarantContactDetailsFromACC14: Option[ContactDetails] =
    getLeadDisplayDeclaration.flatMap(_.getDeclarantDetails.contactDetails)

  final def isConsigneePostCodeFromAcc14: Option[Boolean] =
    getLeadDisplayDeclaration.map(
      _.getConsigneeDetails.exists(_.establishmentAddress.postalCode.isEmpty)
    )

  final def isDeclarantPostCodeFromAcc14: Option[Boolean] =
    getLeadDisplayDeclaration.map(_.getDeclarantDetails).map(_.establishmentAddress.postalCode.isEmpty)

  /** Check if ACC14 have declarant EORI or consignee EORI matching user's EORI */
  final def needsDeclarantAndConsigneeEoriSubmission: Boolean =
    !(userHasGBEoriMatchingDeclaration || userHasXIEoriMatchingDeclaration)

  final def hasSubmittedDeclarantAndConsigneeEori: Boolean =
    answers.eoriNumbersVerification
      .exists(d =>
        d.declarantEoriNumber.isDefined &&
          (!getConsigneeEoriFromACC14.isDefined || d.consigneeEoriNumber.isDefined)
      )

  final def needsUserXiEoriSubmission: Boolean =
    !userHasGBEoriMatchingDeclaration &&
      getLeadDisplayDeclaration.exists(_.containsXiEori) &&
      answers.eoriNumbersVerification.flatMap(_.userXiEori).isEmpty

  final def userHasGBEoriMatchingDeclaration: Boolean =
    getDeclarantEoriFromACC14.contains(answers.userEoriNumber) ||
      getConsigneeEoriFromACC14.contains(answers.userEoriNumber)

  final def userHasXIEoriMatchingDeclaration: Boolean =
    answers.eoriNumbersVerification.exists(x =>
      x.hasSameXiEoriAs(getDeclarantEoriFromACC14) ||
        x.hasSameXiEoriAs(getConsigneeEoriFromACC14)
    )

  final def needsDeclarantAndConsigneePostCode: Boolean =
    !isConsigneePostCodeFromAcc14.getOrElse(false) &&
      !isDeclarantPostCodeFromAcc14.getOrElse(false)

  final def getConsigneeBankAccountDetails: Option[BankAccountDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.displayResponseDetail.bankDetails.flatMap(_.consigneeBankDetails))

  final def getDeclarantBankAccountDetails: Option[BankAccountDetails] =
    getLeadDisplayDeclaration
      .flatMap(_.displayResponseDetail.bankDetails.flatMap(_.declarantBankDetails))

  final def computeBankAccountDetails: Option[BankAccountDetails] =
    answers.bankAccountDetails match {
      case Some(details) => Some(details)
      case None          =>
        val maybeDeclarantBankDetails       = getDeclarantBankAccountDetails
        val maybeConsigneeBankDetails       = getConsigneeBankAccountDetails
        val consigneeAndDeclarantEorisMatch = (for
          consigneeEori <- getConsigneeEoriFromACC14
          declarantEori <- getDeclarantEoriFromACC14
        yield consigneeEori === declarantEori).getOrElse(false)

        (answers.payeeType, maybeDeclarantBankDetails, maybeConsigneeBankDetails) match {
          case (Some(PayeeType.Consignee), _, Some(consigneeBankDetails))                                       =>
            Some(consigneeBankDetails)
          case (Some(PayeeType.Declarant), Some(declarantBankDetails), _)                                       =>
            Some(declarantBankDetails)
          case (Some(PayeeType.Declarant), None, Some(consigneeBankDetails)) if consigneeAndDeclarantEorisMatch =>
            Some(consigneeBankDetails)
          case (Some(PayeeType.Consignee), Some(declarantBankDetails), None) if consigneeAndDeclarantEorisMatch =>
            Some(declarantBankDetails)
          case _                                                                                                => None
        }
    }

  final def toReimbursementWithCorrectAmount(
    claims: SortedMap[TaxCode, Option[AmountPaidWithCorrect]]
  ): List[ReimbursementWithCorrectAmount] =
    claims.view
      .map {
        case (taxCode, Some(amount)) =>
          Some(
            ReimbursementWithCorrectAmount(
              taxCode,
              amount.paidAmount - amount.correctAmount,
              amount.paidAmount,
              amount.correctAmount
            )
          )
        case _                       => None
      }
      .collect { case Some(x) => x }
      .toList

  final def getInitialBankAccountDetailsFromDeclaration: Option[BankAccountDetails] =
    getConsigneeBankAccountDetails.orElse(getDeclarantBankAccountDetails)

  final def needsProofOfAuthorityForBankAccountDetailsChange: Boolean =
    answers.bankAccountDetails.forall { bankDetails =>
      getConsigneeBankAccountDetails.forall(_ =!= bankDetails) &&
      getDeclarantBankAccountDetails.forall(_ =!= bankDetails)
    }

  final def haveBankDetailsOnAcc14: Boolean =
    getLeadDisplayDeclaration
      .exists(_.hasBankDetails)

  final def getClaimantType: ClaimantType =
    if getConsigneeEoriFromACC14.exists(eori =>
        answers.userEoriNumber === eori ||
          answers.eoriNumbersVerification.exists(_.hasSameXiEoriAs(eori))
      )
    then ClaimantType.Consignee
    else if getDeclarantEoriFromACC14.exists(eori =>
        answers.userEoriNumber === eori ||
          answers.eoriNumbersVerification.exists(_.hasSameXiEoriAs(eori))
      )
    then ClaimantType.Declarant
    else ClaimantType.User

  final def getClaimantEori: Eori = getClaimantType match {
    case ClaimantType.Consignee => getConsigneeEoriFromACC14.getOrElse(answers.userEoriNumber)
    case ClaimantType.Declarant => getDeclarantEoriFromACC14.getOrElse(answers.userEoriNumber)
    case ClaimantType.User      => answers.userEoriNumber
  }

  final def getClaimantInformation: Option[ClaimantInformation] =
    for
      contactDetails <- answers.contactDetails
      contactAddress <- answers.contactAddress
    yield ClaimantInformation.from(
      getClaimantEori,
      getClaimantType match {
        case ClaimantType.Consignee => getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails)
        case ClaimantType.Declarant => getLeadDisplayDeclaration.map(_.getDeclarantDetails)
        case ClaimantType.User      => getLeadDisplayDeclaration.map(_.getDeclarantDetails)
      },
      contactDetails,
      contactAddress
    )

  final def journeyDurationSeconds: Long =
    Instant.now().getEpochSecond() - startTimeSeconds

  final def emailAddressHasChanged: Boolean =
    answers.contactDetails.exists(_.emailAddressHasChanged)

  final def contactNameHasChanged: Boolean =
    answers.contactDetails.exists(_.nameHasChanged)

  final def phoneNumberHasChanged: Boolean =
    answers.contactDetails.exists(_.phoneNumberHasChanged)

  final def contactAddressHasChanged: Boolean =
    answers.contactAddress.exists(_.addressHasChanged)

  final def bankAccountHasChanged: Boolean =
    answers.bankAccountDetails.exists(_.bankAccountHasChanged)

  final def computeAddressDetails: Option[ContactAddress] =
    answers.contactAddress

  final def getInitialAddressDetailsFromDeclaration: Option[ContactAddress] = (
    getLeadDisplayDeclaration.flatMap(_.getConsigneeDetails),
    getLeadDisplayDeclaration.map(_.getDeclarantDetails)
  ) match {
    case (Some(consigneeDetails), _) if getConsigneeEoriFromACC14.contains(answers.userEoriNumber) =>
      Some(consigneeDetails.establishmentAddress.toContactAddress)
    case (_, Some(declarantDetails))                                                               =>
      Some(declarantDetails.establishmentAddress.toContactAddress)
    case _                                                                                         => None
  }

  protected def nextAfter[A](item: A)(seq: Seq[A]): Option[A] = {
    val i = seq.indexOf(item)
    if i === -1 then None
    else if i === seq.size - 1 then None
    else Some(seq(i + 1))
  }

  final def declarationsHasOnlySubsidyPayments: Boolean =
    getDisplayDeclarations.nonEmpty &&
      getDisplayDeclarations.forall(_.hasOnlySubsidyPayments)

  def containsUnsupportedTaxCode: Boolean =
    getLeadDisplayDeclaration.map(_.containsSomeUnsupportedTaxCode).getOrElse(false)

  def getPayeeTypeForOutput(payeeTypeOpt: Option[PayeeType]): Option[PayeeType] =
    payeeTypeOpt.map(payeeType => if payeeType === Representative then Declarant else payeeType)

}
