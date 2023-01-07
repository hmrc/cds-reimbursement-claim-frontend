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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import cats.implicits.catsSyntaxEq
import cats.implicits.catsSyntaxOptionId
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen.arbitraryBankAccountDetailsGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen.arbitraryBankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BasisOfClaimGen.arbitraryBasisOfClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimedReimbursementsAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimedReimbursementsAnswerGen.arbitraryClaimedReimbursementsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.AdditionalDetailsGen.arbitraryCompleteAdditionalDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genContactAddressOpt
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen.genMrnContactDetailsOpt
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen.arbitraryDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutiesSelectedAnswerGen.arbitraryDutiesSelectedAnswerGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReimbursementMethodGen.arbitraryReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen.arbitrarySupportingEvidenceDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen.arbitrarySupportingEvidenceAnswerList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen.genScheduledDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.YesNoGen.arbitraryYesNo

import java.util.UUID

object DraftClaimGen {

  def genValidDraftClaim(typeOfClaim: TypeOfClaimAnswer): Gen[DraftClaim] =
    for {
      mrn                         <- genMRN
      maybeContactDetails         <- genMrnContactDetailsOpt
      maybeContactAddressAnswer   <- genContactAddressOpt
      bankAccountDetailsAnswer    <- arbitraryBankAccountDetailsGen.arbitrary
      bankAccountTypeAnswer       <- arbitraryBankAccountType.arbitrary
      basisOfClaim                <- arbitraryBasisOfClaim.arbitrary
      documentTypeAnswer          <- arbitrarySupportingEvidenceDocumentType.arbitrary
      supportingEvidencesAnswer   <- arbitrarySupportingEvidenceAnswerList.arbitrary
      dutiesSelectedAnswer        <- arbitraryDutiesSelectedAnswerGen.arbitrary
      additionalDetailsAnswer     <- arbitraryCompleteAdditionalDetailsAnswer.arbitrary
      whetherNorthernIrelandClaim <- arbitraryYesNo.arbitrary
      displayDeclaration          <- arbitraryDisplayDeclaration.arbitrary
      eori                        <- arbitraryEori.arbitrary
      claimedReimbursementsAnswer <- arbitraryClaimedReimbursementsAnswer.arbitrary
      reimbursementMethod         <- arbitraryReimbursementMethod.arbitrary
      scheduledDocumentAnswer     <- genScheduledDocumentAnswer(typeOfClaim)
      associatedMRNsAnswer        <- genAssociatedMrnsAnswer(typeOfClaim)
      associatedMRNsClaimsAnswer  <- genAssociatedMRNsClaimsAnswer(typeOfClaim)
    } yield DraftClaim(
      id = UUID.randomUUID(),
      typeOfClaim = typeOfClaim.some,
      movementReferenceNumber = mrn.some,
      mrnContactDetailsAnswer = maybeContactDetails,
      mrnContactAddressAnswer = maybeContactAddressAnswer,
      bankAccountDetailsAnswer = bankAccountDetailsAnswer.some,
      bankAccountTypeAnswer = bankAccountTypeAnswer.some,
      basisOfClaimAnswer = basisOfClaim.some,
      documentTypeAnswer = documentTypeAnswer.some,
      supportingEvidencesAnswer = supportingEvidencesAnswer.some,
      dutiesSelectedAnswer = dutiesSelectedAnswer.some,
      additionalDetailsAnswer = additionalDetailsAnswer.some,
      whetherNorthernIrelandAnswer = whetherNorthernIrelandClaim.some,
      displayDeclaration = displayDeclaration.some,
      importerEoriNumberAnswer = answers.ImporterEoriNumberAnswer(eori).some,
      declarantEoriNumberAnswer = DeclarantEoriNumberAnswer(eori).some,
      claimedReimbursementsAnswer = claimedReimbursementsAnswer.some,
      reimbursementMethodAnswer =
        if (typeOfClaim === TypeOfClaimAnswer.Individual)
          reimbursementMethod
        else None,
      scheduledDocumentAnswer = scheduledDocumentAnswer,
      associatedMRNsAnswer = associatedMRNsAnswer,
      associatedMRNsClaimsAnswer = associatedMRNsClaimsAnswer
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
    if (answer === TypeOfClaimAnswer.Multiple)
      arbitraryAssociatedMRNsAnswer.arbitrary.map(_.some)
    else None

  def genAssociatedMRNsClaimsAnswer(answer: TypeOfClaimAnswer): Gen[Option[AssociatedMRNsClaimsAnswer]] =
    if (answer === TypeOfClaimAnswer.Multiple)
      arbitraryAssociatedMRNsClaimsAnswer.arbitrary.map(_.some)
    else None
}
