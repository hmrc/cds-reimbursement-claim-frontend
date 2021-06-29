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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model

import cats.Eq
//import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetailsAnswer.CompleteBankAccountDetailAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaimAnswer.CompleteBasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CommoditiesDetailsAnswer.CompleteCommodityDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CompleteNorthernIrelandAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ContactDetailsAnswer.CompleteContactDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantEoriNumberAnswer.CompleteDeclarantEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.CompleteDeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationDetailsAnswer.CompleteDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DetailsRegisteredWithCdsAnswer.CompleteDetailsRegisteredWithCdsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DuplicateDeclarationDetailsAnswer.CompleteDuplicateDeclarationDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ImporterEoriNumberAnswer.CompleteImporterEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonAndBasisOfClaimAnswer.CompleteReasonAndBasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, SupportingEvidenceAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

import java.util.UUID

final case class CompleteClaim[T <: ReferenceNumber](
  id: UUID,
  movementReferenceNumber: T,
  maybeDuplicateMovementReferenceNumberAnswer: Option[T],
  maybeCompleteDeclarationDetailsAnswer: Option[CompleteDeclarationDetailsAnswer],
  maybeCompleteDuplicateDeclarationDetailsAnswer: Option[CompleteDuplicateDeclarationDetailsAnswer],
  completeDeclarantTypeAnswer: CompleteDeclarantTypeAnswer,
  completeDetailsRegisteredWithCdsAnswer: CompleteDetailsRegisteredWithCdsAnswer,
  maybeContactDetailsAnswer: Option[CompleteContactDetailsAnswer],
  maybeBasisOfClaimAnswer: Option[CompleteBasisOfClaimAnswer],
  maybeCompleteBankAccountDetailAnswer: Option[CompleteBankAccountDetailAnswer],
  supportingEvidenceAnswer: SupportingEvidenceAnswer,
  completeCommodityDetailsAnswer: CompleteCommodityDetailsAnswer,
  completeNorthernIrelandAnswer: Option[CompleteNorthernIrelandAnswer],
  maybeCompleteReasonAndBasisOfClaimAnswer: Option[CompleteReasonAndBasisOfClaimAnswer],
  maybeDisplayDeclaration: Option[DisplayDeclaration],
  maybeDuplicateDisplayDeclaration: Option[DisplayDeclaration],
  importerEoriNumber: Option[CompleteImporterEoriNumberAnswer],
  declarantEoriNumber: Option[CompleteDeclarantEoriNumberAnswer],
  claimsAnswer: ClaimsAnswer
) extends Claim[T]

object CompleteClaim {

  implicit def eq[T <: ReferenceNumber]: Eq[CompleteClaim[T]] = Eq.fromUniversalEquals
  //implicit def format[T <: ReferenceNumber]: OFormat[CompleteClaim[T]] = Json.format
}
