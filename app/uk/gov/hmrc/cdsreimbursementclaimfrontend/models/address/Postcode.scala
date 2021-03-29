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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address

import java.util.function.Predicate
import play.api.data.Forms.{nonEmptyText, text}
import play.api.data.{FormError, Forms, Mapping}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.libs.functional.syntax._
import play.api.libs.json.Format
import cats.implicits.catsSyntaxEq
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Postcode.{mappingNonUk, mappingUk}

final case class Postcode(value: String) extends AnyVal

object Postcode {

  val postcodeRegexPredicate: Predicate[String] =
    "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,3}$".r.pattern
      .asPredicate()

  implicit val format: Format[Postcode] =
    implicitly[Format[String]].inmap(Postcode(_), _.value)

  val mappingUk: Mapping[Postcode] = {

    def validateUkPostcode(p: Postcode): ValidationResult = {
      val postcodeWithoutSpaces = p.value.toUpperCase.replaceAllLiterally(" ", "")
      if (p.value.length > 8) Invalid("error.tooLong")
      else if (!postcodeWithoutSpaces.forall(_.isLetterOrDigit))
        Invalid("error.invalidCharacters")
      else if (!postcodeRegexPredicate.test(postcodeWithoutSpaces))
        Invalid("error.pattern")
      else Valid
    }

    nonEmptyText(maxLength = 9)
      .transform[Postcode](p => Postcode(p.trim), _.value)
      .verifying(Constraint[Postcode](validateUkPostcode(_)))
  }

  val mappingNonUk: Mapping[Postcode] = text(maxLength = 9).transform[Postcode](p => Postcode(p.trim), _.value)

  val mapping: Mapping[String] = PostCodeMapping("", "countryCode", "GB", Nil)
    .transform[String](
      _.value,
      Postcode(_)
    )
}

final case class PostCodeMapping(
  key: String,
  conditionFieldName: String,
  conditionFieldValue: String,
  constraints: Seq[Constraint[Postcode]] = Nil
) extends Mapping[Postcode] {

  override val mappings: Seq[Mapping[_]] = Seq(this)

  override def bind(data: Map[String, String]): Either[Seq[FormError], Postcode] = {
    val condition = data.get(conditionFieldName).map(_ === conditionFieldValue).getOrElse(false)
    val mapping   = if (condition) mappingUk else mappingNonUk
    Forms.single(key -> mapping).bind(data)
  }
  override def unbind(value: Postcode): Map[String, String] = Map(key -> value.value)

  override def unbindAndValidate(value: Postcode): (Map[String, String], Seq[FormError]) =
    this.unbind(value) -> collectErrors(value)

  override def withPrefix(prefix: String): Mapping[Postcode] =
    addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(additionalConstraints: Constraint[Postcode]*): Mapping[Postcode] =
    this.copy(constraints = additionalConstraints ++ constraints)
}
