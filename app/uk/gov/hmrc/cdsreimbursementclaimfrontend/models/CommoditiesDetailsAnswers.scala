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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait CommoditiesDetailsAnswers extends Product with Serializable

object CommoditiesDetailsAnswers {

  final case class IncompleteCommoditiesDetailsAnswers(
    commodityDetails: Option[CommodityDetails]
  ) extends CommoditiesDetailsAnswers

  object IncompleteCommoditiesDetailsAnswers {
    val empty: IncompleteCommoditiesDetailsAnswers = IncompleteCommoditiesDetailsAnswers(None)

    implicit val format: OFormat[IncompleteCommoditiesDetailsAnswers] =
      derived.oformat[IncompleteCommoditiesDetailsAnswers]()
  }

  final case class CompleteCommodityDetailsAnswers(
    commodityDetails: CommodityDetails
  ) extends CommoditiesDetailsAnswers

  object CompleteCommodityDetailsAnswers {
    implicit val format: OFormat[CompleteCommodityDetailsAnswers] =
      derived.oformat[CompleteCommodityDetailsAnswers]()
  }

  implicit class CommoditiesDetailsOps(
    private val a: CommoditiesDetailsAnswers
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteCommoditiesDetailsAnswers => A,
      ifComplete: CompleteCommodityDetailsAnswers => A
    ): A =
      a match {
        case i: IncompleteCommoditiesDetailsAnswers => ifIncomplete(i)
        case c: CompleteCommodityDetailsAnswers     => ifComplete(c)
      }
  }

  implicit val format: OFormat[CommoditiesDetailsAnswers] =
    derived.oformat[CommoditiesDetailsAnswers]()
}
