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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

final case class DuplicateDeclarationVerificationStatus private (flags: Flags) {

  def isVerified: Boolean =
    isImporterEoriVerified && isDeclarantEoriVerified

  def isImporterEoriVerified: Boolean  = flags.check(0)
  def isDeclarantEoriVerified: Boolean = flags.check(1)

  def withConsigneeEoriVerified: DuplicateDeclarationVerificationStatus =
    DuplicateDeclarationVerificationStatus(flags.set(0))

  def withDeclarantEoriVerified: DuplicateDeclarationVerificationStatus =
    DuplicateDeclarationVerificationStatus(flags.set(1))

}

object DuplicateDeclarationVerificationStatus {

  val unverified: DuplicateDeclarationVerificationStatus =
    new DuplicateDeclarationVerificationStatus(Flags.empty)

  val verified: DuplicateDeclarationVerificationStatus =
    new DuplicateDeclarationVerificationStatus(Flags.parse("11"))

  implicit val format: Format[DuplicateDeclarationVerificationStatus] =
    SimpleStringFormat(s => DuplicateDeclarationVerificationStatus(Flags.parse(s)), _.flags.toString)
}
