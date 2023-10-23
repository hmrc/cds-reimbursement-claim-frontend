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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat

sealed trait PayeeType

/** The type of a user filling out the claim. */
object PayeeType extends EnumerationFormat[PayeeType] {

  /** Importer of the goods. */
  case object Consignee extends PayeeType

  /** A representative (a.k.a. Agent) who has submitted original import declaration. */
  case object Declarant extends PayeeType

  override val values: Set[PayeeType] = Set(Consignee, Declarant)
}
