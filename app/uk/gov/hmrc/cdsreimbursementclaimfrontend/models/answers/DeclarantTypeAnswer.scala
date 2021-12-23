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
import cats.Id
import cats.implicits.catsSyntaxOption
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.MissingAnswerError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.Validator

sealed trait DeclarantTypeAnswer

object DeclarantTypeAnswer {

  case object Importer extends DeclarantTypeAnswer //consignee
  case object AssociatedWithImporterCompany extends DeclarantTypeAnswer //consignee
  case object AssociatedWithRepresentativeCompany extends DeclarantTypeAnswer //declarant

  val all: Set[DeclarantTypeAnswer] =
    Set(Importer, AssociatedWithImporterCompany, AssociatedWithRepresentativeCompany)

  val validator: Validator[Id, DeclarantTypeAnswer] = (maybeDeclarantType: Option[DeclarantTypeAnswer]) =>
    maybeDeclarantType.toValidNel(MissingAnswerError("Declarant type"))

  implicit val equality: Eq[DeclarantTypeAnswer] =
    Eq.fromUniversalEquals[DeclarantTypeAnswer]

  implicit val format: OFormat[DeclarantTypeAnswer] =
    derived.oformat[DeclarantTypeAnswer]()
}
