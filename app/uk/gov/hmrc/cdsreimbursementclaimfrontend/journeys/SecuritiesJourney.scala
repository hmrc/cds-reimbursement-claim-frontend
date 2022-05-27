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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DEC91ResponseDetail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantInformation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SecuritiesReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import com.github.arturopala.validator.Validator

final class SecuritiesJourney private (
  val answers: SecuritiesJourney.Answers,
  val caseNumber: Option[String] = None
) extends FluentSyntax[SecuritiesJourney] {

  /** Check if the journey is ready to finalize, i.e. to get the output. */
  def hasCompleteAnswers: Boolean =
    SecuritiesJourney.validator.apply(this).isValid

  def isFinalized: Boolean = caseNumber.isDefined

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[List[String], SecuritiesJourney.Output] =
    Left(Nil)

}

object SecuritiesJourney {

  /** A starting point to build new instance of the journey. */
  def empty(userEoriNumber: Eori, nonce: Nonce = Nonce.random): SecuritiesJourney =
    new SecuritiesJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce))

  type SecuritiesReclaims = Map[TaxCode, Option[BigDecimal]]

  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    reasonForSecurity: Option[ReasonForSecurity] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    selectedSecurityDepositIds: Seq[String] = Seq.empty,
    exportMovementReferenceNumber: Option[MRN] =
      None, // mandatory if reasonForSecurity is T/A, see ReasonForSecurity.requiresExportDeclaration
    exportDeclaration: Option[DEC91ResponseDetail] = None, // mandatory as above
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    reclaimingFullAmount: Option[Boolean] = None,
    securitiesReclaims: Option[SecuritiesReclaims] = None, // mandatory if NOT reclaimingFullAmount
    selectedDocumentType: Option[UploadDocumentType] = None, // ??? depending on the RfS and ....
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    reimbursementMethod: Option[SecuritiesReimbursementMethod] = None, // mandatory if guarantee is eligible
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    checkYourAnswersChangeMode: Boolean = false
  )

  final case class Output(
    movementReferenceNumber: MRN,
    claimantType: ClaimantType,
    claimantInformation: ClaimantInformation,
    reasonForSecurity: ReasonForSecurity,
    securitiesReclaims: Map[TaxCode, BigDecimal],
    reimbursementMethod: SecuritiesReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import com.github.arturopala.validator.Validator._

  val validator: Validate[SecuritiesJourney] =
    Validator.always

}
