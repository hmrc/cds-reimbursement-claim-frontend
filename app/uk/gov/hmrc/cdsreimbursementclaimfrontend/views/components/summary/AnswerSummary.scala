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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList

import scala.collection.SortedMap

trait AnswerSummary[A] {
  def render(key: String, answer: A)(implicit
    subKey: Option[String],
    journey: JourneyBindable,
    messages: Messages
  ): SummaryList
}

object AnswerSummary {

  implicit val claimTypeSummary: AnswerSummary[DeclarantTypeAnswer]                                      = ClaimTypeSummary
  implicit val basisOfClaimSummary: AnswerSummary[BasisOfClaimAnswer]                                    = BasisOfClaimSummary
  implicit val bankAccountDetailsSummary: AnswerSummary[BankAccountDetails]                              = BankAccountDetailsSummary
  implicit val multipleClaimsAnswerSummary: AnswerSummary[List[(MRN, ClaimedReimbursementsAnswer)]]      =
    MultipleClaimsAnswerSummary
  implicit val mrnSummary: AnswerSummary[MRN]                                                            = MovementReferenceNumberSummary
  implicit val mrnsSummary: AnswerSummary[List[MRN]]                                                     = MovementReferenceNumbersSummary
  implicit val commodityDetailsSummary: AnswerSummary[CommodityDetailsAnswer]                            = CommodityDetailsSummary
  implicit val cdsDisplayDeclarationSummary: AnswerSummary[DisplayDeclaration]                           = CdsDisplayDeclarationSummary
  implicit val northernIrelandAnswerSummary: AnswerSummary[YesNo]                                        = NorthernIrelandAnswerSummary
  implicit val reimbursementMethodAnswerSummary: AnswerSummary[ReimbursementMethodAnswer]                =
    ReimbursementMethodAnswerSummary
  implicit val reimbursementsSummary: AnswerSummary[SelectedDutyTaxCodesReimbursementAnswer]             =
    DutyAndTaxCodeReimbursementSummary
  implicit val claimedReimbursementsSummary: AnswerSummary[ClaimedReimbursementsAnswer]                  =
    ClaimedReimbursementsAnswerSummary
  implicit val taxCodeReimbursementSummary: AnswerSummary[(DutyType, SortedMap[TaxCode, Reimbursement])] =
    TaxCodeReimbursementSummary
  implicit val cdsClaimantDetailsSummary: AnswerSummary[
    (
      NamePhoneEmail,
      EstablishmentAddress,
      Option[MrnContactDetails],
      Option[ContactAddress]
    )
  ]                                                                                                      = CdsClaimantDetailsSummary
  implicit val supportingEvidenceSummary: AnswerSummary[SupportingEvidencesAnswer]                       = SupportingEvidenceSummary
  implicit val scheduledDocumentSummary: AnswerSummary[ScheduledDocumentAnswer]                          = ScheduledDocumentSummary

}
