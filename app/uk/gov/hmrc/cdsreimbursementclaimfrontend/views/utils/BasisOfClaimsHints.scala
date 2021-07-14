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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.utils

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim

sealed trait BasisOfClaimsHints {
  val items: Seq[Int]

  def buildLabelKey(parentKey: String, index: Int): String
  def buildTextKey(parentKey: String, index: Int): String
}

object BasisOfClaimsHints {

  final case class MrnBasisOfClaimsHints(items: Seq[Int]) extends BasisOfClaimsHints {
    def buildLabelKey(parentKey: String, index: Int): String =
      s"$parentKey.details.b$index"

    def buildTextKey(parentKey: String, index: Int): String =
      s"$parentKey.details.l$index"
  }

  final case class EntryNumberBasisOfClaimsHints(items: Seq[Int]) extends BasisOfClaimsHints {
    def buildLabelKey(parentKey: String, index: Int): String =
      s"$parentKey.details.ern.b$index"

    def buildTextKey(parentKey: String, index: Int): String =
      s"$parentKey.details.ern.l$index"
  }

  def of(draftClaim: DraftClaim): Builder = Builder(draftClaim.isMrnFlow)

  final case class Builder(isMrnFlow: Boolean) {

    def skip(n: Int): BasisOfClaimsHints = {
      val items = (0 to 13).drop(n)
      if (isMrnFlow) MrnBasisOfClaimsHints(items)
      else EntryNumberBasisOfClaimsHints(items)
    }
  }
}
