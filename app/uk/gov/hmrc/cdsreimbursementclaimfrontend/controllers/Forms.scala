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
import play.api.data.Form
import play.api.data.Forms.seq
import play.api.data.Forms.list
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.data.Forms.optional
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutiesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import play.api.data.Mapping
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils.moneyMapping

object Forms {
  def eoriNumberForm(key: String): Form[Eori] = Form(
    mapping(
      key -> nonEmptyText(maxLength = 18)
        .verifying("invalid.number", str => str.length > 18 || str.isEmpty || Eori(str).isValid)
    )(Eori.apply)(Eori.unapply)
  )

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

  val bankAccountTypeForm: Form[BankAccountType] =
    Form(
      mapping(
        "select-bank-account-type" -> nonEmptyText.verifying(BankAccountType.keys.contains _)
      )(BankAccountType.tryParse)(md => Some(BankAccountType.keyOf(md)))
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

  def claimAmountForm(key: String, paidAmount: BigDecimal): Form[BigDecimal] =
    Form(
      mapping(
        s"$key.claim-amount" -> moneyMapping(
          precision = 13,
          scale = 2,
          errorMsg = s"error.invalid-text",
          allowZero = false,
          zeroErrorMsg = Some(s"error.zero")
        ).verifying("error.invalid-amount", amount => amount >= 0 && amount < paidAmount)
      )(amount => amount)(amount => Some(amount))
    )
}
