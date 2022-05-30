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

import cats.Eq
import cats.syntax.eq._
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FluentSyntax
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DEC91Response
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MapFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat
import SecuritiesJourney.Answers
import scala.collection.immutable.SortedMap

final class SecuritiesJourney private (
  val answers: SecuritiesJourney.Answers,
  val caseNumber: Option[String] = None
) extends Claim[SecuritiesJourney]
    with CommonJourneyProperties
    with FluentSyntax[SecuritiesJourney] {

  final override def getLeadMovementReferenceNumber: Option[MRN] =
    answers.movementReferenceNumber

  final override def getLeadDisplayDeclaration: Option[DisplayDeclaration] =
    answers.displayDeclaration

  final def getSecurityDepositIds: Seq[String] =
    getLeadDisplayDeclaration
      .flatMap(_.getSecurityDepositIds)
      .getOrElse(Seq.empty)

  /** Resets the journey with the new MRN
    * or keep an existing journey if submitted the same MRN.
    */
  final def submitMovementReferenceNumber(
    mrn: MRN
  ): SecuritiesJourney =
    whileClaimIsAmendable {
      getLeadMovementReferenceNumber match {
        case Some(existingMrn) if existingMrn === mrn =>
          this

        case _ =>
          new SecuritiesJourney(
            Answers(
              userEoriNumber = answers.userEoriNumber,
              movementReferenceNumber = Some(mrn),
              nonce = answers.nonce
            )
          )
      }
    }

  final def submitReasonForSecurityAndDeclaration(
    reasonForSecurity: ReasonForSecurity,
    displayDeclaration: DisplayDeclaration
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (!answers.movementReferenceNumber.contains(displayDeclaration.getMRN))
        Left("submitReasonForSecurityAndDeclaration.wrongDisplayDeclarationMrn")
      else if (!displayDeclaration.getReasonForSecurity.contains(reasonForSecurity))
        Left("submitReasonForSecurityAndDeclaration.wrongDisplayDeclarationRfS")
      else if (
        answers.reasonForSecurity.contains(reasonForSecurity) &&
        answers.displayDeclaration.contains(displayDeclaration)
      ) Right(this) // unchanged
      else
        Right(
          new SecuritiesJourney(
            answers.copy(
              reasonForSecurity = Some(reasonForSecurity),
              displayDeclaration = Some(displayDeclaration)
            )
          )
        )
    }

  final def selectSecurityDepositIds(securityDepositIds: Seq[String]): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      if (securityDepositIds.isEmpty)
        Left("selectSecurityDepositIds.emptySelection")
      else if (securityDepositIds.forall(getSecurityDepositIds.contains(_)))
        Right(
          new SecuritiesJourney(
            answers.copy(
              selectedSecurityDepositIds = securityDepositIds
            )
          )
        )
      else Left("selectSecurityDepositIds.invalidSecurityDepositId")
    }

  final def submitExportMovementReferenceNumberAndDeclaration(
    exportMrn: MRN,
    exportDeclaration: DEC91Response
  ): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      Right(
        new SecuritiesJourney(
          answers.copy(
            exportMovementReferenceNumber = Some(exportMrn),
            exportDeclaration = Some(exportDeclaration)
          )
        )
      )
    }

  final def finalizeJourneyWith(caseNumber: String): Either[String, SecuritiesJourney] =
    whileClaimIsAmendable {
      validate(this).toEither
        .fold(
          errors => Left(errors.headOption.getOrElse("completeWith.invalidJourney")),
          _ => Right(new SecuritiesJourney(answers = this.answers, caseNumber = Some(caseNumber)))
        )
    }

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[SecuritiesJourney]) {
      val that = obj.asInstanceOf[SecuritiesJourney]
      that.answers === this.answers && that.caseNumber === this.caseNumber
    } else false

  override def hashCode(): Int    = answers.hashCode
  override def toString(): String = s"SecuritiesJourney($answers,$caseNumber)"

  /** Validates the journey and retrieves the output. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def toOutput: Either[List[String], SecuritiesJourney.Output] =
    Left(Nil)

}

object SecuritiesJourney {

  /** A starting point to build new instance of the journey. */
  def empty(userEoriNumber: Eori, nonce: Nonce = Nonce.random): SecuritiesJourney =
    new SecuritiesJourney(Answers(userEoriNumber = userEoriNumber, nonce = nonce))

  type SecuritiesReclaims = SortedMap[TaxCode, Option[BigDecimal]]

  final case class Answers(
    nonce: Nonce = Nonce.random,
    userEoriNumber: Eori,
    movementReferenceNumber: Option[MRN] = None,
    reasonForSecurity: Option[ReasonForSecurity] = None,
    displayDeclaration: Option[DisplayDeclaration] = None,
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    selectedSecurityDepositIds: Seq[String] = Seq.empty,
    exportMovementReferenceNumber: Option[MRN] =
      None, // mandatory if reasonForSecurity is T/A, see ReasonForSecurity.requiresExportDeclaration
    exportDeclaration: Option[DEC91Response] = None, // mandatory as above
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    reclaimingFullAmount: Option[Boolean] = None,
    securitiesReclaims: Option[SortedMap[String, SecuritiesReclaims]] = None, // mandatory if NOT reclaimingFullAmount
    selectedDocumentType: Option[UploadDocumentType] = None, // ??? depending on the RfS and ....
    supportingEvidences: Seq[UploadedFile] = Seq.empty,
    reimbursementMethod: Option[SecuritiesReimbursementMethod] = None, // mandatory if guarantee is eligible
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    checkYourAnswersChangeMode: Boolean = false
  ) extends CommonAnswers

  final case class Output(
    movementReferenceNumber: MRN,
    claimantType: ClaimantType,
    claimantInformation: ClaimantInformation,
    reasonForSecurity: ReasonForSecurity,
    securitiesReclaims: SortedMap[String, SortedMap[TaxCode, BigDecimal]],
    reimbursementMethod: SecuritiesReimbursementMethod,
    bankAccountDetails: Option[BankAccountDetails],
    supportingEvidences: Seq[EvidenceDocument]
  )

  import com.github.arturopala.validator.Validator._

  implicit val validator: Validate[SecuritiesJourney] =
    Validator.never

  object Answers {
    implicit lazy val mapFormat1: Format[SortedMap[TaxCode, Option[BigDecimal]]] =
      MapFormat.formatSortedWithOptionalValue[TaxCode, BigDecimal]

    implicit lazy val mapFormat2: Format[SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]]] =
      MapFormat.formatSorted[String, SortedMap[TaxCode, Option[BigDecimal]]]

    implicit lazy val mapFormat3: Format[Map[UploadDocumentType, (Nonce, Seq[UploadedFile])]] =
      MapFormat.format[UploadDocumentType, (Nonce, Seq[UploadedFile])]

    implicit val amountFormat: Format[BigDecimal] =
      SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

    implicit val equality: Eq[Answers]   = Eq.fromUniversalEquals[Answers]
    implicit val format: Format[Answers] = Json.using[Json.WithDefaultValues].format[Answers]
  }

  object Output {

    implicit lazy val mapFormat1: Format[SortedMap[TaxCode, BigDecimal]] =
      MapFormat.formatSorted[TaxCode, BigDecimal]

    implicit lazy val mapFormat2: Format[SortedMap[String, SortedMap[TaxCode, BigDecimal]]] =
      MapFormat.formatSorted[String, SortedMap[TaxCode, BigDecimal]]

    implicit val amountFormat: Format[BigDecimal] =
      SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

    implicit val equality: Eq[Output]   = Eq.fromUniversalEquals[Output]
    implicit val format: Format[Output] = Json.format[Output]
  }

  import play.api.libs.functional.syntax._

  implicit val format: Format[SecuritiesJourney] =
    Format(
      ((JsPath \ "answers").read[Answers]
        and (JsPath \ "caseNumber").readNullable[String])(new SecuritiesJourney(_, _)),
      ((JsPath \ "answers").write[Answers]
        and (JsPath \ "caseNumber").writeNullable[String])(journey => (journey.answers, journey.caseNumber))
    )

  implicit val equality: Eq[SecuritiesJourney] =
    Eq.fromUniversalEquals[SecuritiesJourney]

}
