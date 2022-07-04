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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._

trait SecuritiesJourneyTestData extends JourneyTestData {

  val emptyJourney: SecuritiesJourney =
    SecuritiesJourney.empty(exampleEori)

  def tryBuildSecuritiesJourney(
    userEoriNumber: Eori,
    mrn: MRN,
    reasonForSecurity: ReasonForSecurity,
    displayDeclaration: DisplayDeclaration,
    similarClaimExistAlreadyInCDFPay: Boolean,
    reclaims: Seq[(String, TaxCode, BigDecimal)],
    exportMrn: Option[MRN],
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    supportingEvidences: Map[UploadDocumentType, Int] = Map.empty
  ): Either[String, SecuritiesJourney] = {

    val supportingEvidencesExpanded: Map[UploadDocumentType, Seq[UploadedFile]] =
      supportingEvidences.map { case (documentType, size) =>
        (documentType, (0 until size).map(i => buildUploadDocument(s"$i")))
      }

    def receiveUploadedFiles(journey: SecuritiesJourney)(
      documentTypeAndUploadedFiles: (UploadDocumentType, Seq[UploadedFile])
    ): Either[String, SecuritiesJourney] = {
      val (documentType, uploadedFiles) = documentTypeAndUploadedFiles
      val allUploadedFiles              = journey.answers.supportingEvidences ++ uploadedFiles
      journey.receiveUploadedFiles(documentType, journey.answers.nonce, allUploadedFiles)
    }

    SecuritiesJourney
      .empty(userEoriNumber)
      .submitMovementReferenceNumber(mrn)
      .submitReasonForSecurityAndDeclaration(reasonForSecurity, displayDeclaration)
      .flatMap(_.submitClaimDuplicateCheckStatus(similarClaimExistAlreadyInCDFPay))
      .tryWhenDefined(exportMrn)(journey => (exportMrn => journey.submitExportMovementReferenceNumber(exportMrn)))
      .flatMapWhenDefined(consigneeEoriNumber)(_.submitConsigneeEoriNumber _)
      .flatMapWhenDefined(declarantEoriNumber)(_.submitDeclarantEoriNumber _)
      .map(_.submitContactDetails(contactDetails))
      .mapWhenDefined(contactAddress)(_.submitContactAddress _)
      .flatMapEach(reclaims.map(_._1).distinct, (journey: SecuritiesJourney) => journey.selectSecurityDepositId(_))
      .flatMapEach(
        reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq,
        (journey: SecuritiesJourney) =>
          (args: (String, Seq[(TaxCode, BigDecimal)])) =>
            journey
              .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
              .flatMapEach(
                args._2,
                (journey: SecuritiesJourney) =>
                  (args2: (TaxCode, BigDecimal)) => journey.submitAmountForReclaim(args._1, args2._1, args2._2)
              )
      )
      .flatMapWhenDefined(bankAccountDetails)(_.submitBankAccountDetails _)
      .flatMapWhenDefined(bankAccountType)(_.submitBankAccountType _)
      .flatMapEach(supportingEvidencesExpanded, receiveUploadedFiles)
      .map(_.submitCheckYourAnswersChangeMode(true))
  }

}
