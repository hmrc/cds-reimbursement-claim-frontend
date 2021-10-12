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

import cats.implicits.catsSyntaxOptionId
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.CheckDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimController.CheckClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDeclarantEoriNumberController.DeclarantEoriNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterImporterEoriNumberController.ImporterEoriNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantEoriNumberAnswer.CompleteDeclarantEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ImporterEoriNumberAnswer.CompleteImporterEoriNumberAnswer
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen.{arbitrarySupportingEvidenceAnswer, genScheduledDocumentAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountType, DraftClaim, MovementReferenceNumber, SelectNumberOfClaimsAnswer}

import java.util.UUID

object DraftClaimGen extends HigherPriorityDraftClaimGen

trait HigherPriorityDraftClaimGen extends LowerPriorityDraftClaimGen {
  implicit val arbitraryDraftClaimGen: Typeclass[DraftClaim] = gen[DraftClaim]
}

trait LowerPriorityDraftClaimGen {

  def genValidDraftClaim(selectNumberOfClaimsAnswer: SelectNumberOfClaimsAnswer): Gen[DraftC285Claim] =
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
      checkClaimAnswer               <- gen[CheckClaimAnswer].arbitrary
      checkDeclarationDetailsAnswer  <- gen[CheckDeclarationDetailsAnswer].arbitrary
      scheduledDocumentAnswer        <- genScheduledDocumentAnswer(selectNumberOfClaimsAnswer)
      associatedMRNsAnswer           <- arbitraryAssociatedMRNsAnswer.arbitrary
    } yield DraftC285Claim(
      id = UUID.randomUUID(),
      selectNumberOfClaimsAnswer = selectNumberOfClaimsAnswer.some,
      movementReferenceNumber = MovementReferenceNumber(mrn).some,
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
      importerEoriNumberAnswer = CompleteImporterEoriNumberAnswer(ImporterEoriNumber(eori)).some,
      declarantEoriNumberAnswer = CompleteDeclarantEoriNumberAnswer(DeclarantEoriNumber(eori)).some,
      claimsAnswer = claimsAnswer.some,
      checkClaimAnswer = checkClaimAnswer.some,
      checkDeclarationDetailsAnswer = checkDeclarationDetailsAnswer.some,
      scheduledDocumentAnswer = scheduledDocumentAnswer,
      associatedMRNsAnswer = associatedMRNsAnswer.some
    )

  implicit val arbitraryDraftC285Claim: Typeclass[DraftC285Claim] = Arbitrary {
    for {
      numberOfClaims <- gen[SelectNumberOfClaimsAnswer].arbitrary
      claim          <- genValidDraftClaim(numberOfClaims)
    } yield claim
  }
}
