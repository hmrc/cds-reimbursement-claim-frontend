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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import cats.implicits.catsSyntaxEq
import play.api.data.Forms._
import play.api.data.Form
import play.api.data.Mapping
import play.api.data.validation.Constraint
import play.api.data.validation.Invalid
import play.api.data.validation.Valid
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.NorthernIrelandController.dataKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithRefund
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsJourneyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SortCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer.BankAccountTransfer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer.CurrentMonthAdjustment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AdditionalDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutiesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils.moneyMapping
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.TimeUtils

object Forms {

  def eoriNumberForm(key: String): Form[Eori] = Form(
    mapping(
      key -> nonEmptyText(maxLength = 18)
        .verifying("invalid.number", str => str.length > 18 || str.isEmpty || Eori(str).isValid)
    )(Eori.apply)(Eori.unapply)
  )

  val chooseHowManyMrnsForm: Form[RejectedGoodsJourneyType] = Form(
    mapping(
      "rejected-goods.choose-how-many-mrns" -> nonEmptyText
        .verifying("error.required", journey => journey.isEmpty || RejectedGoodsJourneyType.has(journey))
    )(RejectedGoodsJourneyType.findUnsafe)(borgc => Option(borgc.toString))
  )

  val northernIrelandForm: Form[YesNo] = YesOrNoQuestionForm(dataKey)

  val selectSecuritiesForm: Form[YesNo] = YesOrNoQuestionForm("select-securities")

  val basisOfRejectedGoodsClaimForm: Form[BasisOfRejectedGoodsClaim] = Form(
    mapping(
      "select-basis-for-claim.rejected-goods" -> nonEmptyText
        .verifying("error.required", basis => basis.isEmpty || BasisOfRejectedGoodsClaim.has(basis))
    )(BasisOfRejectedGoodsClaim.findUnsafe)(borgc => Option(borgc.toString))
  )

  val enterSpecialCircumstancesForm: Form[String] = Form(
    "enter-special-circumstances.rejected-goods" -> nonEmptyText(maxLength = 500)
  )

  val mrnContactDetailsForm: Form[MrnContactDetails] = Form(
    mapping(
      "enter-contact-details.contact-name"         -> nonEmptyText(maxLength = 512),
      "enter-contact-details.contact-email"        -> Email.mappingMaxLength,
      "enter-contact-details.contact-phone-number" -> optional(PhoneNumber.mapping)
    )(MrnContactDetails.apply)(MrnContactDetails.unapply)
  )

  val methodOfDisposalForm: Form[MethodOfDisposal] =
    Form(
      mapping(
        "select-method-of-disposal.rejected-goods" -> nonEmptyText.verifying(MethodOfDisposal.keys.contains _)
      )(MethodOfDisposal.tryParse)(md => Some(MethodOfDisposal.keyOf(md)))
    )

  val enterInspectionDateForm: Form[InspectionDate] = {
    val key: String = "enter-inspection-date.rejected-goods"
    Form(
      mapping(
        "" -> of(
          TimeUtils.dateFormatter(s"$key.day", s"$key.month", s"$key.year", key)
        )
      )(InspectionDate(_))(d => Some(d.value))
    )
  }

  val bankAccountTypeForm: Form[BankAccountType] =
    Form(
      mapping(
        "select-bank-account-type" -> nonEmptyText.verifying(BankAccountType.keys.contains _)
      )(BankAccountType.tryParse)(md => Some(BankAccountType.keyOf(md)))
    )

  val accountNumberMapping: Mapping[AccountNumber] =
    nonEmptyText
      .transform[String](s => s.replaceAll("[-( )]+", ""), identity)
      .verifying("error.length", str => AccountNumber.hasValidLength(str))
      .verifying("error.invalid", str => AccountNumber.isValid(str))
      .transform[AccountNumber](
        s => {
          val paddedNumber = s.reverse.padTo(8, '0').reverse
          AccountNumber(paddedNumber)
        },
        _.value
      )

  val sortCodeMapping: Mapping[SortCode] =
    nonEmptyText
      .transform[SortCode](s => SortCode(s.replaceAll("[-( )]+", "")), _.value)
      .verifying("invalid", e => SortCode.isValid(e.value))

  val accountNameMapping: Mapping[AccountName] =
    nonEmptyText(maxLength = 40)
      .transform[AccountName](s => AccountName(s.trim()), _.value)
      .verifying("invalid", e => AccountName.isValid(e.value))

  val enterBankDetailsForm: Form[BankAccountDetails] = Form(
    mapping(
      "enter-bank-details.account-name"   -> accountNameMapping,
      "enter-bank-details.sort-code"      -> sortCodeMapping,
      "enter-bank-details.account-number" -> accountNumberMapping
    )(BankAccountDetails.apply)(BankAccountDetails.unapply)
  )

  val rejectedGoodsContactDetailsForm: Form[MrnContactDetails] = Form(
    mapping(
      "enter-contact-details-rejected-goods.contact-name"         -> nonEmptyText(maxLength = 512),
      "enter-contact-details-rejected-goods.contact-email"        -> Email.mappingMaxLength,
      "enter-contact-details-rejected-goods.contact-phone-number" -> optional(PhoneNumber.mapping)
    )(MrnContactDetails.apply)(MrnContactDetails.unapply)
  )

  val enterRejectedGoodsDetailsForm: Form[String] = Form(
    "enter-rejected-goods-details.rejected-goods" -> nonEmptyText(maxLength = 500)
  )

  val additionalDetailsForm: Form[AdditionalDetailsAnswer] = Form(
    mapping("enter-additional-details" -> nonEmptyText(maxLength = 500))(AdditionalDetailsAnswer.apply)(
      AdditionalDetailsAnswer.unapply
    )
  )

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def selectDutiesForm(allAvailableDuties: DutiesSelectedAnswer): Form[DutiesSelectedAnswer] = Form(
    mapping(
      "select-duties" -> list(
        mapping(
          "" -> nonEmptyText
            .verifying(
              "invalid tax code",
              code => allAvailableDuties.map(_.taxCode.value).exists(_ === code)
            )
            .transform[TaxCode](
              (x: String) => TaxCodes.findUnsafe(x),
              (t: TaxCode) => t.value
            )
        )(Duty.apply)(Duty.unapply)
      ).verifying("error.required", _.nonEmpty)
    )(taxCodes => DutiesSelectedAnswer(taxCodes.head, taxCodes.tail: _*))(dsa => Some(dsa.toList))
  )

  val selectDutyTypesForm: Form[List[DutyType]] = Form(
    mapping(
      "select-duty-types" -> list(
        mapping(
          "" -> nonEmptyText
            .verifying(
              "error.invalid",
              code => DutyTypes has code
            )
        )(DutyType.apply)(DutyType.unapply)
      ).verifying("error.required", _.nonEmpty)
    )(identity)(Some(_))
  )

  def taxCodeMapping(availableTaxCodes: Seq[TaxCode]): Mapping[TaxCode] =
    nonEmptyText
      .verifying(
        "invalid tax code",
        code => availableTaxCodes.exists(_.value === code)
      )
      .transform[TaxCode](
        (x: String) => TaxCodes.findUnsafe(x),
        (t: TaxCode) => t.value
      )

  def selectTaxCodesForm(availableTaxCodes: Seq[TaxCode]): Form[Seq[TaxCode]] = {
    val taxCode = taxCodeMapping(availableTaxCodes)
    Form(
      mapping(
        "select-duties" -> seq(taxCode).verifying("error.required", _.nonEmpty)
      )(identity)(seq => Some(seq))
    )
  }

  val selectDutyCodesForm: Form[List[TaxCode]] =
    Form(
      mapping(
        "select-duty-codes" -> list(
          mapping(
            "" -> nonEmptyText
              .verifying(
                "error.invalid",
                code => TaxCodes has code
              )
          )(TaxCode.apply)(TaxCode.unapply)
        ).verifying("error.required", _.nonEmpty)
      )(identity)(Some(_))
    )

  val enterScheduledClaimForm: Form[AmountPaidWithCorrect] = Form(
    "enter-scheduled-claim" ->
      mapping(
        "paid-amount"   -> moneyMapping(13, 2, "error.invalid", zeroErrorMsg = Some(s"error.zero")),
        "actual-amount" -> moneyMapping(13, 2, "error.invalid", allowZero = true)
      )(AmountPaidWithCorrect.apply)(AmountPaidWithCorrect.unapply)
        .verifying(
          "invalid.claim",
          _.isValid
        )
  )

  val enterScheduledClaimRejectedGoodsForm: Form[AmountPaidWithRefund] = Form(
    "enter-claim-scheduled.rejected-goods" ->
      mapping(
        "paid-amount"  -> moneyMapping(13, 2, "error.invalid", zeroErrorMsg = Some(s"error.zero")),
        "claim-amount" -> moneyMapping(13, 2, "error.invalid", zeroErrorMsg = Some(s"error.zero"))
      )(AmountPaidWithRefund.apply)(AmountPaidWithRefund.unapply)
        .verifying(
          "invalid.claim",
          _.isValid
        )
  )

  val inspectionAddressTypeForm: Form[InspectionAddressType] =
    Form(
      "inspection-address.type" ->
        nonEmptyText
          .verifying("error.invalid", s => InspectionAddressType.hasKey(s))
          .transform[InspectionAddressType](InspectionAddressType.tryParse, InspectionAddressType.keyOf)
    )

  def claimAmountForm(key: String, paidAmount: BigDecimal): Form[BigDecimal] =
    Form(
      mapping(
        s"$key.claim-amount" -> moneyMapping(
          precision = 13,
          scale = 2,
          errorMsg = s"error.invalid-text",
          zeroErrorMsg = Some(s"error.zero")
        ).verifying("error.invalid-amount", amount => amount >= 0 && amount <= paidAmount)
      )(amount => amount)(amount => Some(amount))
    )

  def reimbursementMethodForm(reimbursementMethodKey: String): Form[ReimbursementMethodAnswer] =
    Form(
      mapping(
        reimbursementMethodKey -> number
          .verifying("error.invalid", a => a === 0 || a === 1)
          .transform[ReimbursementMethodAnswer](
            value =>
              if (value === 0) CurrentMonthAdjustment
              else BankAccountTransfer,
            {
              case CurrentMonthAdjustment => 0
              case BankAccountTransfer    => 1
            }
          )
      )(identity)(Some(_))
    )

  val none: String = "none"

  def chooseFileTypeForm(
    availableFileTypes: Set[UploadDocumentType]
  ): Form[Option[UploadDocumentType]] =
    Form(
      mapping(
        "choose-file-type" -> nonEmptyText
          .verifying(
            "choose-file-type.error.invalid-file-type",
            key =>
              key === none || UploadDocumentType.parse(key).map(v => availableFileTypes.contains(v)).getOrElse(false)
          )
          .transform[Option[UploadDocumentType]](
            (key: String) => if (key === none) None else UploadDocumentType.parse(key),
            (value: Option[UploadDocumentType]) => value.map(UploadDocumentType.keyOf).getOrElse(none)
          )
      )(identity)(x => Some(x))
    )

  def chooseSupportEvidenceDocumentTypeForm(
    documentTypeList: Seq[UploadDocumentType]
  ): Form[UploadDocumentType] =
    Form(
      mapping(
        "supporting-evidence.choose-document-type" -> nonEmptyText
          .verifying(
            "error.invalid-document-type",
            key => UploadDocumentType.parse(key).exists(v => documentTypeList.contains(v))
          )
      )(UploadDocumentType.tryParse)(dt => Some(UploadDocumentType.keyOf(dt)))
    )

  val movementReferenceNumberForm: Form[MRN] =
    Form(
      mapping(
        "enter-movement-reference-number" ->
          nonEmptyText
            .verifying(
              "invalid.number",
              str => str.isEmpty || MRN(str).isValid
            )
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )

  val enterDuplicateMrnWithNoCheck: Form[MRN] =
    Form(
      mapping(
        "enter-duplicate-movement-reference-number" -> nonEmptyText
          .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )

  def enterDuplicateMrnCheckingAgainst(mainMrn: MRN): Form[MRN] =
    Form(
      mapping(
        "enter-duplicate-movement-reference-number" ->
          nonEmptyText
            .verifying(Constraint[String] { str: String =>
              if (str.isEmpty) Invalid("error.required")
              else if (str === mainMrn.value) Invalid("invalid.enter-different-mrn")
              else if (MRN(str).isValid) Valid
              else Invalid("invalid.number")
            })
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )

  val reasonForClaimForm: Form[BasisOfClaimAnswer] =
    Form(
      mapping(
        "select-basis-for-claim" -> number
          .verifying("invalid reason for claim", idx => BasisOfClaims.contains(idx))
          .transform[BasisOfClaimAnswer](BasisOfClaims.all(_), BasisOfClaims.indexOf)
      )(identity)(Some(_))
    )

  val reasonForSecurityForm: Form[ReasonForSecurity] =
    Form(
      mapping(
        "choose-reason-for-security.securities" -> nonEmptyText
          .verifying("error.required", _.nonEmpty)
      )(ReasonForSecurity.findUnsafe)(rfs => Option(rfs.toString))
    )
}
