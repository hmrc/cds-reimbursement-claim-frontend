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
import org.scalacheck.magnolia._
import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.AssociatedMRNsAnswerGen.arbitraryAssociatedMRNsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen.arbitraryBankAccountDetailsGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BasisOfClaimAnswerGen.arbitraryBasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimedReimbursementsAnswerGen.arbitraryClaimedReimbursementsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.CommoditiesDetailsGen.arbitraryCompleteCommodityDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genContactAddressOpt
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen.genMrnContactDetailsOpt
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DeclarantTypeAnswerGen.arbitraryDeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DetailsRegisteredWithCdsAnswerGen.arbitraryDetailsRegisteredWithCds
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen.arbitraryDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutiesSelectedAnswerGen.arbitraryDutiesSelectedAnswerGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.NorthernIrelandAnswerGen.arbitraryNorthernIrelandAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReimbursementMethodAnswerGen.arbitraryReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen.{arbitrarySupportingEvidenceAnswer, genScheduledDocument}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountType, DraftClaim, answers}

import java.util.UUID

object DraftClaimGen {

  def genValidDraftClaim(typeOfClaim: TypeOfClaimAnswer): Gen[DraftClaim] =
    for {
      mrn                            <- genMRN
      declarantType                  <- arbitraryDeclarantTypeAnswer.arbitrary
      detailsRegisteredWithCdsAnswer <- arbitraryDetailsRegisteredWithCds.arbitrary
      maybeContactDetails            <- genMrnContactDetailsOpt
      maybeContactAddressAnswer      <- genContactAddressOpt
      bankAccountDetailsAnswer       <- arbitraryBankAccountDetailsGen.arbitrary
      bankAccountTypeAnswer          <- gen[BankAccountType].arbitrary
      basisOfClaimAnswer             <- arbitraryBasisOfClaimAnswer.arbitrary
      supportingEvidencesAnswer      <- arbitrarySupportingEvidenceAnswer.arbitrary
      dutiesSelectedAnswer           <- arbitraryDutiesSelectedAnswerGen.arbitrary
      commoditiesDetailsAnswer       <- arbitraryCompleteCommodityDetailsAnswer.arbitrary
      claimNorthernIrelandAnswer     <- arbitraryNorthernIrelandAnswer.arbitrary
      displayDeclaration             <- arbitraryDisplayDeclaration.arbitrary
      eori                           <- arbitraryEori.arbitrary
      claimedReimbursementsAnswer    <- arbitraryClaimedReimbursementsAnswer.arbitrary
      reimbursementMethodAnswer      <- arbitraryReimbursementMethodAnswer.arbitrary
      scheduledDocumentAnswer        <- genScheduledDocumentAnswer(typeOfClaim)
      associatedMRNsAnswer           <- genAssociatedMrnsAnswer(typeOfClaim)
    } yield DraftClaim(
      id = UUID.randomUUID(),
      typeOfClaim = typeOfClaim.some,
      movementReferenceNumber = mrn.some,
      declarantTypeAnswer = declarantType.some,
      detailsRegisteredWithCdsAnswer = detailsRegisteredWithCdsAnswer.some,
      mrnContactDetailsAnswer = maybeContactDetails,
      mrnContactAddressAnswer = maybeContactAddressAnswer,
      bankAccountDetailsAnswer = bankAccountDetailsAnswer.some,
      bankAccountTypeAnswer = bankAccountTypeAnswer.some,
      basisOfClaimAnswer = basisOfClaimAnswer.some,
      supportingEvidencesAnswer = supportingEvidencesAnswer.some,
      dutiesSelectedAnswer = dutiesSelectedAnswer.some,
      commoditiesDetailsAnswer = commoditiesDetailsAnswer.some,
      claimNorthernIrelandAnswer = claimNorthernIrelandAnswer.some,
      displayDeclaration = displayDeclaration.some,
      importerEoriNumberAnswer = answers.ImporterEoriNumberAnswer(eori).some,
      declarantEoriNumberAnswer = DeclarantEoriNumberAnswer(eori).some,
      claimedReimbursementsAnswer = claimedReimbursementsAnswer.some,
      reimbursementMethodAnswer =
        if (typeOfClaim === TypeOfClaimAnswer.Individual)
          reimbursementMethodAnswer
        else None,
      scheduledDocumentAnswer = scheduledDocumentAnswer,
      associatedMRNsAnswer = associatedMRNsAnswer
    )

  implicit lazy val arbitraryDraftC285Claim: Typeclass[DraftClaim] = Arbitrary {
    for {
      numberOfClaims <- gen[TypeOfClaimAnswer].arbitrary
      claim          <- genValidDraftClaim(numberOfClaims)
    } yield claim
  }

  def genScheduledDocumentAnswer(answer: TypeOfClaimAnswer): Gen[Option[ScheduledDocumentAnswer]] =
    if (answer === TypeOfClaimAnswer.Scheduled)
      genScheduledDocument.map(Some(_))
    else None

  def genAssociatedMrnsAnswer(answer: TypeOfClaimAnswer): Gen[Option[AssociatedMRNsAnswer]] =
    if (answer === TypeOfClaimAnswer.Multiple) {
      arbitraryAssociatedMRNsAnswer.arbitrary.map(_.some)
    } else None
}
