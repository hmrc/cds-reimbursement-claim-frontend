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
import play.api.data.Forms._
import play.api.data.Form
import play.api.data.Mapping
import play.api.data.validation.Constraint
import play.api.data.validation.Invalid
import play.api.data.validation.Valid
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.NorthernIrelandController.dataKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AccountNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithRefund
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsJourneyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsJourneyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SortCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod.BankAccountTransfer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod.CurrentMonthAdjustment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AdditionalDetailsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaimsList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutiesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
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

  val overpaymentsChooseHowManyMrnsForm: Form[OverpaymentsJourneyType] = Form(
    mapping(
      "overpayments.choose-how-many-mrns" -> nonEmptyText
        .verifying("error.required", journey => journey.isEmpty || OverpaymentsJourneyType.has(journey))
    )(OverpaymentsJourneyType.findUnsafe)(borgc => Option(borgc.toString))
  )

  val northernIrelandForm: Form[YesNo] = YesOrNoQuestionForm(dataKey)

  val selectSecuritiesForm: Form[YesNo] = YesOrNoQuestionForm("select-securities")

  val selectBillOfDischargeForm: Form[YesNo] = YesOrNoQuestionForm("bill-of-discharge")

  val basisOfRejectedGoodsClaimForm: Form[BasisOfRejectedGoodsClaim] = Form(
    mapping(
      "select-basis-for-claim.rejected-goods" -> nonEmptyText
        .verifying("error.required", basis => basis.isEmpty || BasisOfRejectedGoodsClaim.has(basis))
    )(BasisOfRejectedGoodsClaim.findUnsafe)(borgc => Option(borgc.toString))
  )

  val bankAccountLetterOfAuthorityForm: Form[YesNo] = YesOrNoQuestionForm("bank_account_letter_of_authority")

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
      "enter-bank-account-details.account-name"   -> accountNameMapping,
      "enter-bank-account-details.sort-code"      -> sortCodeMapping,
      "enter-bank-account-details.account-number" -> accountNumberMapping
    )(BankAccountDetails.apply)(BankAccountDetails.unapply)
  )

  val enterRejectedGoodsDetailsForm: Form[String] = Form(
    "enter-rejected-goods-details.rejected-goods" -> nonEmptyText(maxLength = 500)
  )

  val additionalDetailsForm: Form[AdditionalDetailsAnswer] = Form(
    mapping("enter-additional-details" -> nonEmptyText(maxLength = 500))(AdditionalDetailsAnswer.apply)(
      AdditionalDetailsAnswer.unapply
    )
  )

  val confirmFullRepaymentForm: Form[YesNo]                                    = YesOrNoQuestionForm("confirm-full-repayment")
  val chooseExportMethodForm: Form[Option[TemporaryAdmissionMethodOfDisposal]] = Form(
    mapping(
      "choose-export-method" -> nonEmptyText
        .transform[Option[models.TemporaryAdmissionMethodOfDisposal]](
          (key: String) => if (key === noneString) None else TemporaryAdmissionMethodOfDisposal.parse(key),
          (value: Option[models.TemporaryAdmissionMethodOfDisposal]) =>
            value.map(TemporaryAdmissionMethodOfDisposal.keyOf).getOrElse(noneString)
        )
    )(identity)(x => Some(x))
  )

  val checkTotalImportDischargedForm: Form[YesNo] = YesOrNoQuestionForm("check-total-import-discharged")

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
        "paid-amount"   -> moneyMapping("error.invalid", zeroErrorMsg = Some(s"error.zero")),
        "actual-amount" -> moneyMapping("error.invalid", allowZero = true)
      )(AmountPaidWithCorrect.apply)(AmountPaidWithCorrect.unapply)
        .verifying(
          "invalid.claim",
          _.isValid
        )
  )

  val enterScheduledClaimRejectedGoodsForm: Form[AmountPaidWithRefund] = Form(
    "enter-claim-scheduled.rejected-goods" ->
      mapping(
        "paid-amount"  -> moneyMapping("error.invalid", zeroErrorMsg = Some(s"error.zero")),
        "claim-amount" -> moneyMapping("error.invalid", zeroErrorMsg = Some(s"error.zero"))
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
          errorMsg = s"error.invalid-text",
          zeroErrorMsg = Some(s"error.zero")
        ).verifying("error.invalid-amount", _ <= paidAmount)
      )(amount => amount)(Some(_))
    )

  def reimbursementMethodForm(reimbursementMethodKey: String): Form[ReimbursementMethod] =
    Form(
      mapping(
        reimbursementMethodKey -> number
          .verifying("error.invalid", a => a === 0 || a === 1)
          .transform[ReimbursementMethod](
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
            (key: String) => if (key === noneString) None else UploadDocumentType.parse(key),
            (value: Option[UploadDocumentType]) => value.map(UploadDocumentType.keyOf).getOrElse(noneString)
          )
      )(identity)(Some(_))
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
              if (str === mainMrn.value) Invalid("invalid.enter-different-mrn")
              else if (str.nonEmpty && !MRN(str).isValid) Invalid("invalid.number")
              else Valid
            })
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )

  val basisOfOverpaymentClaimForm: Form[BasisOfOverpaymentClaim] =
    Form(
      mapping(
        "select-basis-for-claim" -> number
          .verifying("invalid reason for claim", idx => BasisOfOverpaymentClaimsList.contains(idx))
          .transform[BasisOfOverpaymentClaim](BasisOfOverpaymentClaimsList.all(_), BasisOfOverpaymentClaimsList.indexOf)
      )(identity)(Some(_))
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
      )(ClaimAmount.apply)(ClaimAmount.unapply)
        .verifying("invalid.claim", a => a.amount >= 0 && a.amount < paidAmount)
    )
}
