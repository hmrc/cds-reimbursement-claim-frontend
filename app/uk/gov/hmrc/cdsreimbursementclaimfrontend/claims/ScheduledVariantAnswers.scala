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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.claims

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import scala.collection.immutable.SortedMap

/** Common answers of the scheduled claim variant. */
trait ScheduledVariantAnswers extends CommonAnswers {

  def movementReferenceNumber: Option[MRN]
  def scheduledDocument: Option[UploadedFile]
  def displayDeclaration: Option[DisplayDeclaration]
  def correctedAmounts: Option[SortedMap[DutyType, SortedMap[TaxCode, Option[AmountPaidWithCorrect]]]]
  def exciseCategories: Option[Seq[ExciseCategory]]

}
