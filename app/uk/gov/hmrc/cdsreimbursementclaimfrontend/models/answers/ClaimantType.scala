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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import cats.Eq
import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat

sealed trait ClaimantType

/** The type of a user filling out the claim. */
object ClaimantType {

  /** Importer of the goods. */
  case object Consignee extends ClaimantType

  /** A representative (a.k.a. Agent) who has submitted original import declaration. */
  case object Declarant extends ClaimantType

  /** New representative, not a consignee nor the original declarant. */
  case object User extends ClaimantType

  val all: Set[ClaimantType] = Set(Consignee, Declarant, User)

  implicit val equality: Eq[ClaimantType]   = Eq.fromUniversalEquals[ClaimantType]
  implicit val format: Format[ClaimantType] = EnumerationFormat(all)
}
