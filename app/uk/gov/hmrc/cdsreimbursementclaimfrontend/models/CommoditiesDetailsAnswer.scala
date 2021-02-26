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

sealed trait CommoditiesDetailsAnswer extends Product with Serializable

object CommoditiesDetailsAnswer {

  final case class IncompleteCommoditiesDetailsAnswer(
    commodityDetails: Option[CommodityDetails]
  ) extends CommoditiesDetailsAnswer

  object IncompleteCommoditiesDetailsAnswer {
    val empty: IncompleteCommoditiesDetailsAnswer = IncompleteCommoditiesDetailsAnswer(None)

    implicit val format: OFormat[IncompleteCommoditiesDetailsAnswer] =
      derived.oformat[IncompleteCommoditiesDetailsAnswer]()
  }

  final case class CompleteCommodityDetailsAnswer(
    commodityDetails: CommodityDetails
  ) extends CommoditiesDetailsAnswer

  object CompleteCommodityDetailsAnswer {
    implicit val format: OFormat[CompleteCommodityDetailsAnswer] =
      derived.oformat[CompleteCommodityDetailsAnswer]()
  }

  implicit class CommoditiesDetailsOps(
    private val a: CommoditiesDetailsAnswer
  ) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteCommoditiesDetailsAnswer => A,
      ifComplete: CompleteCommodityDetailsAnswer => A
    ): A =
      a match {
        case i: IncompleteCommoditiesDetailsAnswer => ifIncomplete(i)
        case c: CompleteCommodityDetailsAnswer     => ifComplete(c)
      }
  }

  implicit val format: OFormat[CommoditiesDetailsAnswer] =
    derived.oformat[CommoditiesDetailsAnswer]()
}
