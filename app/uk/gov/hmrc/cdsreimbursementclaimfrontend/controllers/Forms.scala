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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import cats.implicits.catsSyntaxEq
import play.api.data.Forms.*
import play.api.data.format.Formats.*
import play.api.data.format.Formatter
import play.api.data.validation.Constraints.maxLength
import play.api.data.validation.Constraint
import play.api.data.validation.Constraints
import play.api.data.validation.Invalid
import play.api.data.validation.Valid
import play.api.data.Form
import play.api.data.Mapping
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod.BankAccountTransfer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod.CurrentMonthAdjustment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils.moneyMapping
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.TimeUtils

object Forms {

  extension [A](left: Constraint[A]) {
    inline def or(right: Constraint[A]): Constraint[A] =
      Constraint(value =>
        left(value) match {
          case Valid   => right(value)
          case invalid => invalid
        }
      )
  }

  extension [A : Formatter](m: Mapping[A]) {
    inline def sequential: Mapping[A] =
      of[A].verifying(m.constraints.reduce(_.or(_)))
  }

  val eoriNumberMapping: Mapping[String] =
    text
      .verifying(Constraints.nonEmpty)
      .verifying(Constraints.minLength(3))
      .verifying(Constraints.maxLength(17))
      .verifying("invalid.number", str => Eori(str).isValid)
      .sequential

  def eoriNumberForm(key: String): Form[Eori] = Form(
    mapping(
      key -> eoriNumberMapping
    )(Eori.apply)(v => Some(v.value))
  )

  def newDanForm(key: String): Form[Dan] = Form(
    mapping(
      key -> nonEmptyText(maxLength = 7)
        .verifying("invalid.number", str => str.length > 7 || str.isEmpty || Dan(str).isValid)
    )(Dan.apply)(v => Some(v.value))
  )

  val chooseHowManyMrnsForm: Form[RejectedGoodsClaimType] = Form(
    mapping(
      "rejected-goods.choose-how-many-mrns" -> nonEmptyText
        .verifying("error.required", claim => claim.isEmpty || RejectedGoodsClaimType.has(claim))
    )(RejectedGoodsClaimType.findUnsafe)(borgc => Option(borgc.toString))
  )

  val overpaymentsChooseHowManyMrnsForm: Form[OverpaymentsClaimType] = Form(
    mapping(
      "overpayments.choose-how-many-mrns" -> nonEmptyText
        .verifying("error.required", claim => claim.isEmpty || OverpaymentsClaimType.has(claim))
    )(OverpaymentsClaimType.findUnsafe)(borgc => Option(borgc.toString))
  )

  val northernIrelandForm: Form[YesNo] = YesOrNoQuestionForm("claim-northern-ireland")

  val selectSecuritiesForm: Form[YesNo] = YesOrNoQuestionForm("select-securities")

  val partialClaimsForm: Form[YesNo] = YesOrNoQuestionForm("partial-claims")

  val selectBillOfDischargeForm: Form[YesNo] = YesOrNoQuestionForm("bill-of-discharge")

  val basisOfRejectedGoodsClaimForm: Form[BasisOfRejectedGoodsClaim] = Form(
    mapping(
      "select-basis-for-claim.rejected-goods" -> nonEmptyText
        .verifying("error.required", basis => basis.isEmpty || BasisOfRejectedGoodsClaim.has(basis))
    )(BasisOfRejectedGoodsClaim.findUnsafe)(borgc => Option(borgc.toString))
  )

  val enterSpecialCircumstancesForm: Form[String] = Form(
    "enter-special-circumstances.rejected-goods" -> nonEmptyText()
      .transform[String](_.replace("\r\n", "\n"), _.replace("\n", "\r\n"))
      .verifying(maxLength(500))
  )

  val mrnContactDetailsForm: Form[MrnContactDetails] = Form(
    mapping(
      "enter-contact-details.contact-name"         -> nonEmptyText(maxLength = 512),
      "enter-contact-details.contact-email"        -> Email.mappingMaxLength,
      "enter-contact-details.contact-phone-number" -> optional(PhoneNumber.mapping)
    )(MrnContactDetails.apply(_, _, _))(MrnContactDetails.unapply3)
  )

  val methodOfDisposalForm: Form[MethodOfDisposal] =
    Form(
      mapping(
        "select-method-of-disposal.rejected-goods" -> nonEmptyText.verifying(MethodOfDisposal.keys.contains)
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

  val payeeTypeForm: Form[PayeeType] =
    Form(
      mapping(
        "choose-payee-type" -> nonEmptyText.verifying(PayeeType.keys.contains)
      )(PayeeType.tryParse)(p => Some(PayeeType.keyOf(p)))
    )

  val currencyTypeForm: Form[CurrencyType] =
    Form(
      mapping(
        "choose-currency-type" -> nonEmptyText.verifying(CurrencyType.keys.contains)
      )(CurrencyType.tryParse)(p => Some(CurrencyType.keyOf(p)))
    )

  val bankAccountTypeForm: Form[BankAccountType] =
    Form(
      mapping(
        "select-bank-account-type" -> nonEmptyText.verifying(BankAccountType.keys.contains)
      )(BankAccountType.tryParse)(md => Some(BankAccountType.keyOf(md)))
    )

  val accountNumberMapping: Mapping[AccountNumber] =
    nonEmptyText
      .transform[String](s => s.replaceAll("\\D", ""), identity)
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
      .transform[SortCode](s => SortCode(s.replaceAll("\\D", "")), _.value)
      .verifying("invalid", e => SortCode.isValid(e.value))

  val accountNameMapping: Mapping[AccountName] =
    nonEmptyText(maxLength = 70)
      .transform[AccountName](s => AccountName(s.trim()), _.value)
      .verifying("invalid", e => AccountName.isValid(e.value))

  val enterBankDetailsForm: Form[BankAccountDetails] = Form(
    mapping(
      "enter-bank-account-details.account-name"   -> accountNameMapping,
      "enter-bank-account-details.sort-code"      -> sortCodeMapping,
      "enter-bank-account-details.account-number" -> accountNumberMapping
    )(BankAccountDetails.apply(_, _, _))(BankAccountDetails.unapply3)
  )

  val enterRejectedGoodsDetailsForm: Form[String] = Form(
    "enter-rejected-goods-details.rejected-goods" -> nonEmptyText()
      .transform[String](_.replace("\r\n", "\n"), _.replace("\n", "\r\n"))
      .verifying(maxLength(500))
  )

  val enterAdditionalDetailsForm: Form[String] = Form(
    "enter-additional-details" -> nonEmptyText()
      .transform[String](_.replace("\r\n", "\n"), _.replace("\n", "\r\n"))
      .verifying(maxLength(500))
  )

  val enterAdditionalDetailsSecuritiesForm: Form[String] = Form(
    "enter-additional-details" -> text()
      .transform[String](_.replace("\r\n", "\n"), _.replace("\n", "\r\n"))
      .verifying(maxLength(500))
  )

  val confirmFullRepaymentForm: Form[YesNo] = YesOrNoQuestionForm("confirm-full-repayment")

  val chooseExportMethodForm: Form[List[TemporaryAdmissionMethodOfDisposal]] = Form(
    "choose-export-method" -> list(
      mapping(
        "" -> nonEmptyText
          .transform[models.TemporaryAdmissionMethodOfDisposal](
            (key: String) => TemporaryAdmissionMethodOfDisposal.tryParse(key),
            (value: models.TemporaryAdmissionMethodOfDisposal) => TemporaryAdmissionMethodOfDisposal.keyOf(value)
          )
      )(identity)(x => Some(x))
    ).verifying("error.required", _.nonEmpty)
  )

  val checkTotalImportDischargedForm: Form[YesNo] = YesOrNoQuestionForm("check-total-import-discharged")

  val addOtherDocumentsForm: Form[YesNo] = YesOrNoQuestionForm("add-other-documents")

  val selectDutyTypesForm: Form[List[DutyType]] = Form(
    mapping(
      "select-duty-types" -> list(
        mapping(
          "" -> nonEmptyText
            .verifying(
              "error.invalid",
              code => DutyTypes `has` code
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

  def selectDutiesForm(availableTaxCodes: Seq[TaxCode]): Form[Seq[TaxCode]] = {
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
                code => TaxCodes.has(code)
              )
          )(TaxCode.apply)(TaxCode.unapply)
        ).verifying("error.required", _.nonEmpty)
      )(identity)(Some(_))
    )

  val selectExciseCategoriesForm: Form[Seq[ExciseCategory]] =
    Form(
      mapping(
        "select-excise-categories" -> seq(
          mapping(
            "" -> nonEmptyText
              .verifying(
                "error.invalid",
                code => ExciseCategory.has(code)
              )
          )(ExciseCategory.apply)(ExciseCategory.unapply)
        ).verifying("error.required", _.nonEmpty)
      )(identity)(Some(_))
    )

  val enterScheduledClaimForm: Form[AmountPaidWithCorrect] = Form(
    "enter-claim.scheduled" ->
      mapping(
        "paid-amount"  -> moneyMapping("error.invalid", zeroErrorMsg = Some("error.zero")),
        "claim-amount" -> moneyMapping("error.invalid", allowZero = true)
      )(AmountPaidWithCorrect.fromPaidAndClaimAmount)(AmountPaidWithCorrect.toPaidAndClaimAmount)
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

  def actualAmountForm(key: String, paidAmount: BigDecimal): Form[BigDecimal] =
    Form(
      mapping(
        s"$key" -> moneyMapping(
          errorMsg = "actual-amount.error.invalid",
          allowZero = true
        ).verifying("invalid.claim", amount => amount >= 0 && amount < paidAmount)
      )(identity)(Some.apply)
    )

  def claimAmountForm(key: String, paidAmount: BigDecimal): Form[BigDecimal] =
    Form(
      mapping(
        s"$key" -> moneyMapping(
          errorMsg = "error.invalid",
          allowZero = true
        ).verifying("error.amount", amount => amount > 0 && amount <= paidAmount)
      )(identity)(Some.apply)
    )

  def reimbursementMethodForm(reimbursementMethodKey: String): Form[ReimbursementMethod] =
    Form(
      mapping(
        reimbursementMethodKey -> number
          .verifying("error.invalid", a => a === 0 || a === 1)
          .transform[ReimbursementMethod](
            value =>
              if value === 0 then CurrentMonthAdjustment
              else BankAccountTransfer,
            {
              case CurrentMonthAdjustment => 0
              case _                      => 1
            }
          )
      )(identity)(Some(_))
    )

  val noneString: String = "none"

  def chooseFileTypeForm(
    availableFileTypes: Set[UploadDocumentType]
  ): Form[Option[UploadDocumentType]] =
    Form(
      mapping(
        "choose-file-type" -> nonEmptyText
          .verifying(
            "choose-file-type.error.invalid-file-type",
            key => key === noneString || UploadDocumentType.parse(key).exists(v => availableFileTypes.contains(v))
          )
          .transform[Option[UploadDocumentType]](
            (key: String) => if key === noneString then None else UploadDocumentType.parse(key),
            (value: Option[UploadDocumentType]) => value.map(UploadDocumentType.keyOf).getOrElse(noneString)
          )
      )(identity)(Some(_))
    )

  val movementReferenceNumberForm: Form[MRN] =
    Form(
      mapping(
        "enter-movement-reference-number" ->
          text
            .transform[String](_.trim, identity)
            .verifying(Constraint[String] { (str: String) =>
              if str.isBlank then Invalid("error.required")
              else if str.length != 18 then Invalid("invalid.length")
              else if !str.matches("""^\w+$""") then Invalid("invalid.characters")
              else if !MRN(str).isValid then Invalid("invalid.format")
              else Valid
            })
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
          text
            .transform[String](_.trim, identity)
            .verifying(Constraint[String] { (str: String) =>
              if str.isBlank then Invalid("error.required")
              else if str === mainMrn.value then Invalid("invalid.enter-different-mrn")
              else if str.length != 18 then Invalid("invalid.length")
              else if !str.matches("""^\w+$""") then Invalid("invalid.characters")
              else if !MRN(str).isValid then Invalid("invalid.format")
              else Valid
            })
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )

  val firstExportMovementReferenceNumberForm: Form[(MRN, YesNo)] =
    Form(
      mapping(
        "enter-export-movement-reference-number"                   ->
          text
            .transform[String](_.trim, identity)
            .verifying(Constraint[String] { (str: String) =>
              if str.isBlank then Invalid("error.required")
              else if str.length != 18 then Invalid("invalid.length")
              else if !str.matches("""^\w+$""") then Invalid("invalid.characters")
              else if !MRN(str).isValid then Invalid("invalid.format")
              else Valid
            })
            .transform[MRN](MRN(_), _.value),
        "enter-export-movement-reference-number.securities.yes-no" ->
          YesOrNoQuestionForm.yesNoMapping(
            "enter-export-movement-reference-number.securities.yes-no"
          )
      )(Tuple2.apply)(Tuple2.unapply)
    )

  val nextExportMovementReferenceNumberForm: Form[(MRN, Option[YesNo])] =
    Form(
      mapping(
        "enter-export-movement-reference-number.next"              ->
          text
            .transform[String](_.trim, identity)
            .verifying(Constraint[String] { (str: String) =>
              if str.isBlank then Invalid("error.required")
              else if str.length != 18 then Invalid("invalid.length")
              else if !str.matches("""^\w+$""") then Invalid("invalid.characters")
              else if !MRN(str).isValid then Invalid("invalid.format")
              else Valid
            })
            .transform[MRN](MRN(_), _.value),
        "enter-export-movement-reference-number.securities.yes-no" ->
          optional(
            YesOrNoQuestionForm.yesNoMapping(
              "enter-export-movement-reference-number.securities.yes-no"
            )
          )
      )(Tuple2.apply)(Tuple2.unapply)
    )

  val basisOfOverpaymentClaimForm: Form[BasisOfOverpaymentClaim] = Form(
    mapping(
      "select-basis-for-claim" -> nonEmptyText
        .verifying("error.required", basis => basis.isEmpty || BasisOfOverpaymentClaim.has(basis))
    )(BasisOfOverpaymentClaim.findUnsafe)(borgc => Option(borgc.toString))
  )

  val reasonForSecurityForm: Form[ReasonForSecurity] =
    Form(
      mapping(
        "choose-reason-for-security.securities" -> nonEmptyText
          .verifying("error.required", _.nonEmpty)
      )(ReasonForSecurity.findUnsafe)(rfs => Option(rfs.toString))
    )

  def mrnClaimAmountForm(paidAmount: BigDecimal): Form[ClaimAmount] =
    Form(
      mapping(
        "enter-claim" -> moneyMapping("actual-amount.error.invalid", allowZero = true)
      )(ClaimAmount.apply)(v => Some(v.amount))
        .verifying("invalid.claim", a => a.amount >= 0 && a.amount < paidAmount)
    )

  val problemWithDeclarationForm: Form[YesNo] = YesOrNoQuestionForm("problem-with-declaration")

}
