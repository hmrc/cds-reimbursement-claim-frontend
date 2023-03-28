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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile

/** Answers common to the all types of the claim journey. */
trait CommonAnswers {

  def nonce: Nonce
  def userEoriNumber: Eori
  def consigneeEoriNumber: Option[Eori]
  def declarantEoriNumber: Option[Eori]
  def contactDetails: Option[MrnContactDetails]
  def contactAddress: Option[ContactAddress]
  def bankAccountDetails: Option[BankAccountDetails]
  def bankAccountType: Option[BankAccountType]
  def selectedDocumentType: Option[UploadDocumentType]
  def supportingEvidences: Seq[UploadedFile]
  def checkYourAnswersChangeMode: Boolean
}
