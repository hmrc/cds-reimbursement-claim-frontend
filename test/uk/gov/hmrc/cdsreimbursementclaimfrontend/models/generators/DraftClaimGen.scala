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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import cats.implicits.{catsSyntaxEq, catsSyntaxOptionId}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{AssociatedMRNsAnswer, ScheduledDocumentAnswer, SelectNumberOfClaimsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen.arbitraryBankAccountDetailsGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BasisOfClaimAnswerGen.genBasisOfClaimAnswerOpt
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimsAnswerGen.arbitraryClaimsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.CommoditiesDetailsGen.arbitraryCompleteCommodityDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genContactAddressOpt
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen.genMrnContactDetailsOpt
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DeclarantTypeAnswerGen.arbitraryDeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DetailsRegisteredWithCdsAnswerGen.arbitraryDetailsRegisteredWithCds
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen.arbitraryDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutiesSelectedAnswerGen.arbitraryDutiesSelectedAnswerGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.AssociatedMRNsAnswerGen.arbitraryAssociatedMRNsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.NorthernIrelandAnswerGen.arbitraryNorthernIrelandAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReimbursementMethodAnswerGen.arbitraryReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen.arbitrarySupportingEvidenceAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{DeclarantEoriNumber, ImporterEoriNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{UploadDocument, UploadDocumentType}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountType, DraftClaim}

import java.util.UUID

object DraftClaimGen {

  def genValidDraftClaim(selectNumberOfClaimsAnswer: SelectNumberOfClaimsAnswer): Gen[DraftClaim] =
    for {
      mrn                            <- genMRN
      declarantType                  <- arbitraryDeclarantTypeAnswer.arbitrary
      detailsRegisteredWithCdsAnswer <- arbitraryDetailsRegisteredWithCds.arbitrary
      maybeContactDetails            <- genMrnContactDetailsOpt
      maybeContactAddressAnswer      <- genContactAddressOpt
      bankAccountDetailsAnswer       <- arbitraryBankAccountDetailsGen.arbitrary
      bankAccountTypeAnswer          <- gen[BankAccountType].arbitrary
      maybeBasisOfClaimAnswer        <- genBasisOfClaimAnswerOpt
      supportingEvidencesAnswer      <- arbitrarySupportingEvidenceAnswer.arbitrary
      dutiesSelectedAnswer           <- arbitraryDutiesSelectedAnswerGen.arbitrary
      commoditiesDetailsAnswer       <- arbitraryCompleteCommodityDetailsAnswer.arbitrary
      claimNorthernIrelandAnswer     <- arbitraryNorthernIrelandAnswer.arbitrary
      displayDeclaration             <- arbitraryDisplayDeclaration.arbitrary
      eori                           <- arbitraryEori.arbitrary
      claimsAnswer                   <- arbitraryClaimsAnswer.arbitrary
      reimbursementMethodAnswer      <- arbitraryReimbursementMethodAnswer.arbitrary
      scheduledDocumentAnswer        <- genScheduledDocumentAnswer(selectNumberOfClaimsAnswer)
      associatedMRNsAnswer           <- genAssociatedMrnsAnswer(selectNumberOfClaimsAnswer)
    } yield DraftClaim(
      id = UUID.randomUUID(),
      selectNumberOfClaimsAnswer = selectNumberOfClaimsAnswer.some,
      movementReferenceNumber = mrn.some,
      declarantTypeAnswer = declarantType.some,
      detailsRegisteredWithCdsAnswer = detailsRegisteredWithCdsAnswer.some,
      mrnContactDetailsAnswer = maybeContactDetails,
      mrnContactAddressAnswer = maybeContactAddressAnswer,
      bankAccountDetailsAnswer = bankAccountDetailsAnswer.some,
      bankAccountTypeAnswer = bankAccountTypeAnswer.some,
      basisOfClaimAnswer = maybeBasisOfClaimAnswer,
      supportingEvidencesAnswer = supportingEvidencesAnswer.some,
      dutiesSelectedAnswer = dutiesSelectedAnswer.some,
      commoditiesDetailsAnswer = commoditiesDetailsAnswer.some,
      claimNorthernIrelandAnswer = claimNorthernIrelandAnswer.some,
      displayDeclaration = displayDeclaration.some,
      importerEoriNumberAnswer = ImporterEoriNumber(eori).some,
      declarantEoriNumberAnswer = DeclarantEoriNumber(eori).some,
      claimsAnswer = claimsAnswer.some,
      reimbursementMethodAnswer = reimbursementMethodAnswer,
      scheduledDocumentAnswer = scheduledDocumentAnswer,
      associatedMRNsAnswer = associatedMRNsAnswer
    )

  implicit val arbitraryDraftC285Claim: Typeclass[DraftClaim] = Arbitrary {
    for {
      numberOfClaims <- gen[SelectNumberOfClaimsAnswer].arbitrary
      claim          <- genValidDraftClaim(numberOfClaims)
    } yield claim
  }

  def genScheduledDocumentAnswer(answer: SelectNumberOfClaimsAnswer): Gen[Option[ScheduledDocumentAnswer]] =
    if (answer === SelectNumberOfClaimsAnswer.Scheduled)
      gen[UploadDocument].arbitrary.map { doc =>
        Some(ScheduledDocumentAnswer(doc.copy(documentType = Some(UploadDocumentType.ScheduleOfMRNs))))
      }
    else None

  def genAssociatedMrnsAnswer(answer: SelectNumberOfClaimsAnswer): Gen[Option[AssociatedMRNsAnswer]] =
    if (answer === SelectNumberOfClaimsAnswer.Multiple) {
      arbitraryAssociatedMRNsAnswer.arbitrary.map(_.some)
    } else None
}
