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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journeys

import cats.Eq
import cats.syntax.eq._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DocumentTypeRejectedGoods
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import java.time.LocalDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument
import RejectedGoodsSingleJourney.Answers
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MapFormat

object RejectedGoodsSingleJourney {

  // All user answers captured during C&E1179 single MRN journey
  final case class Answers(
    movementReferenceNumber: Option[MRN] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    importerEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    declarantType: Option[DeclarantTypeAnswer] = None, // is it required at all?
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    basisOfClaim: Option[BasisOfRejectedGoodsClaim] = None,
    basisOfClaimSpecialCircumstances: Option[String] = None,
    methodOfDisposal: Option[MethodOfDisposal] = None,
    detailsOfRejectedGoods: Option[String] = None,
    reimbursementClaims: Option[Map[TaxCode, Option[BigDecimal]]] = None,
    inspectionDate: Option[LocalDate] = None,
    inspectionAddress: Option[InspectionAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    reimbursementMethodAnswer: Option[ReimbursementMethodAnswer] = None,
    supportingEvidences: Option[Map[UploadDocument, Option[DocumentTypeRejectedGoods]]] = None
  )

  object Answers {
    implicit lazy val mapFormat1: Format[Map[TaxCode, Option[BigDecimal]]] =
      MapFormat.formatWithOptionalValue[TaxCode, BigDecimal]

    implicit lazy val mapFormat2: Format[Map[UploadDocument, Option[DocumentTypeRejectedGoods]]] =
      MapFormat.formatWithOptionalValue[UploadDocument, DocumentTypeRejectedGoods]

    implicit val equality: Eq[Answers]   = Eq.fromUniversalEquals[Answers]
    implicit val format: Format[Answers] = Json.format[Answers]
  }

}

// A.k.a C&E1179 single MRN journey
final class RejectedGoodsSingleJourney private (val answers: Answers) {

  def isComplete: Boolean = ???

  // resets the journey with the new MRN
  def submitMovementReferenceNumber(mrn: MRN): RejectedGoodsSingleJourney =
    answers.movementReferenceNumber match {
      case Some(existing) if existing === mrn => this
      case _                                  =>
        new RejectedGoodsSingleJourney(Answers(movementReferenceNumber = Some(mrn)))
    }

  // sets the ACC14 declaration and resets all reimbursementClaims
  def submitDisplayDeclaration(displayDeclaration: DisplayDeclaration): RejectedGoodsSingleJourney =
    answers.displayDeclaration match {
      case Some(existing) if existing === displayDeclaration => this
      case _                                                 =>
        new RejectedGoodsSingleJourney(
          answers.copy(displayDeclaration = Some(displayDeclaration), reimbursementClaims = None)
        )
    }

  def submitImporterEoriNumber(importerEoriNumber: Eori): RejectedGoodsSingleJourney =
    answers.importerEoriNumber match {
      case Some(existing) if existing === importerEoriNumber => this
      case _                                                 =>
        new RejectedGoodsSingleJourney(
          answers.copy(importerEoriNumber = Some(importerEoriNumber))
        )
    }

  def submitDeclarantEoriNumber(declarantEoriNumber: Eori): RejectedGoodsSingleJourney =
    answers.declarantEoriNumber match {
      case Some(existing) if existing === declarantEoriNumber => this
      case _                                                  =>
        new RejectedGoodsSingleJourney(
          answers.copy(declarantEoriNumber = Some(declarantEoriNumber))
        )
    }

  def submitDeclarantType(declarantType: DeclarantTypeAnswer): RejectedGoodsSingleJourney =
    answers.declarantType match {
      case Some(existing) if existing === declarantType => this
      case _                                            =>
        new RejectedGoodsSingleJourney(
          answers.copy(declarantType = Some(declarantType))
        )
    }

}
