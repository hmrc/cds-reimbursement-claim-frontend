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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

import cats.Eq
import cats.instances.int.*
import cats.syntax.either.*
import cats.syntax.eq.*
import play.api.data.format.Formats
import play.api.data.format.Formatter
import play.api.data.validation.Constraint
import play.api.data.validation.Invalid
import play.api.data.validation.Valid
import play.api.data.FormError
import play.api.data.Forms
import play.api.data.Mapping

import scala.math.BigDecimal.RoundingMode
import scala.util.Try
import scala.util.control.Exception

object FormUtils {

  def readValue[T](
    key: String,
    data: Map[String, String],
    f: String => T,
    requiredErrorArgs: Seq[String] = Seq.empty
  ): Either[FormError, T] =
    data
      .get(key)
      .map(_.trim())
      .filter(_.nonEmpty)
      .fold[Either[FormError, T]](Left(FormError(key, "error.required", requiredErrorArgs))) { stringValue =>
        Either
          .fromTry(Try(f(stringValue)))
          .leftMap(_ => FormError(key, "error.invalid"))
      }

  def radioFormFormatter[A : Eq](
    orderedOptions: List[A]
  ): Formatter[A] =
    new Formatter[A] {
      val optionsZippedWithIndex: List[(A, Int)] = orderedOptions.zipWithIndex

      override def bind(
        key: String,
        data: Map[String, String]
      ): Either[Seq[FormError], A] = {
        lazy val invalidError = FormError(key, "error.invalid")
        data
          .get(key)
          .map(_.trim())
          .filter(_.nonEmpty)
          .fold[Either[Seq[FormError], A]](
            Left(Seq(FormError(key, "error.required")))
          ) { stringValue =>
            Either
              .fromTry(Try(stringValue.toInt))
              .leftMap(_ => Seq(invalidError))
              .flatMap(i =>
                Either
                  .fromOption(
                    optionsZippedWithIndex.find(_._2 === i),
                    Seq(invalidError)
                  )
                  .map(_._1)
              )
          }
      }

      override def unbind(key: String, value: A): Map[String, String] =
        optionsZippedWithIndex
          .find(_._1 === value)
          .fold(Map.empty[String, String]) { case (_, i) =>
            Map(key -> i.toString)
          }
    }

  def moneyBigDecimalFormat(errorMsg: String): Formatter[BigDecimal] =
    bigDecimalFormat(precision = 13, scale = 2, errorMsg)

  def moneyMapping(
    errorMsg: String = "error.invalid-text",
    allowZero: Boolean = false,
    zeroErrorMsg: Option[String] = None
  ): Mapping[BigDecimal] =
    Forms
      .of[BigDecimal](moneyBigDecimalFormat(errorMsg))
      .verifying(
        Constraint[BigDecimal]((num: BigDecimal) =>
          num match {
            case n if n < 0   => Invalid(errorMsg)
            case n if n === 0 => if allowZero then Valid else Invalid(zeroErrorMsg.getOrElse(errorMsg))
            case _            => Valid
          }
        )
      )

  def bigDecimalFormat(precision: Int, scale: Int, errorMsg: String): Formatter[BigDecimal] =
    new Formatter[BigDecimal] {
      override val format: Option[(String, Nil.type)] = Some(("format.real", Nil))

      def bind(key: String, data: Map[String, String]): Either[Seq[FormError], BigDecimal] =
        Formats.stringFormat.bind(key, data).flatMap { userInput =>
          if userInput.isEmpty then Left(Seq(FormError(key, "error.required")))
          else
            Exception
              .allCatch[BigDecimal]
              .either(
                BigDecimal(
                  userInput
                    .replace("£", "")
                    .replace(" ", "")
                    .replace(",", "")
                )
              )
              .flatMap { bd =>
                if bd.scale > scale then Left(new Throwable("Wrong precision"))
                else if bd.precision - bd.scale > precision - scale then Left(new Throwable("Wrong precision"))
                else Right(bd.setScale(scale, RoundingMode.HALF_UP))
              }
              .leftMap(_ => Seq(FormError(key, errorMsg)))
        }

      def unbind(key: String, value: BigDecimal) =
        Map(key -> value.setScale(scale, RoundingMode.HALF_UP).toString)
    }

}
