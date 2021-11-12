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
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed abstract class TypeOfClaimAnswer(val value: Int) extends Product with Serializable

object TypeOfClaimAnswer {

  case object Individual extends TypeOfClaimAnswer(0)
  case object Multiple extends TypeOfClaimAnswer(1)
  case object Scheduled extends TypeOfClaimAnswer(2)

  val allClaimsTypes: List[TypeOfClaimAnswer]         = List(Individual, Multiple, Scheduled)
  val allClaimsIntToType: Map[Int, TypeOfClaimAnswer] = allClaimsTypes.map(a => a.value -> a).toMap
  val allClaimsTypeToInt: Map[TypeOfClaimAnswer, Int] = allClaimsTypes.map(a => a -> a.value).toMap

  implicit val eq: Eq[TypeOfClaimAnswer]                     = Eq.fromUniversalEquals
  implicit val typeOfClaimFormat: OFormat[TypeOfClaimAnswer] =
    derived.oformat[TypeOfClaimAnswer]()
}
