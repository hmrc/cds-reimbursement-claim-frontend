/*
 * Copyright 2022 HM Revenue & Customs
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

import java.time.format.DateTimeFormatter
import java.time.Clock
import java.time.LocalDate
import java.time.LocalDateTime
import cats.syntax.either._
import configs.ConfigReader
import play.api.data.FormError
import play.api.data.format.Formatter
import scala.util.Try

object TimeUtils {

  implicit val configs: ConfigReader[LocalDate] = ConfigReader.fromTry { case (config, key) =>
    LocalDate.parse(config.getString(key), DateTimeFormatter.ISO_DATE)
  }

  val clock: Clock = Clock.systemUTC()

  val minimumDate: LocalDate = LocalDate.of(1900, 1, 1)

  def today(): LocalDate = LocalDate.now(clock)

  def now(): LocalDateTime = LocalDateTime.now(clock)

  def dateFormatter(
    minimumDateInclusive: Option[LocalDate],
    dayKey: String,
    monthKey: String,
    yearKey: String,
    dateKey: String
  ): Formatter[LocalDate] =
    new Formatter[LocalDate] {
      def dateFieldStringValues(
        data: Map[String, String]
      ): Either[FormError, (String, String, String)] =
        List(dayKey, monthKey, yearKey)
          .map(data.get(_).map(_.trim).filter(_.nonEmpty)) match {
          case Some(dayString) :: Some(monthString) :: Some(
                yearString
              ) :: Nil =>
            Right((dayString, monthString, yearString))
          case None :: Some(_) :: Some(_) :: Nil =>
            Left(FormError(dayKey, "error.required"))
          case Some(_) :: None :: Some(_) :: Nil =>
            Left(FormError(monthKey, "error.required"))
          case Some(_) :: Some(_) :: None :: Nil =>
            Left(FormError(yearKey, "error.required"))
          case Some(_) :: None :: None :: Nil    =>
            Left(FormError(monthKey, "error.monthAndYearRequired"))
          case None :: Some(_) :: None :: Nil    =>
            Left(FormError(dayKey, "error.dayAndYearRequired"))
          case None :: None :: Some(_) :: Nil    =>
            Left(FormError(dayKey, "error.dayAndMonthRequired"))
          case _                                 => Left(FormError(dateKey, "error.required"))
        }

      def toValidInt(
        key: String,
        stringValue: String,
        maxValue: Option[Int]
      ): Either[FormError, Int] =
        Either.fromOption(
          Try(BigDecimal(stringValue).toIntExact).toOption.filter(i => i > 0 && maxValue.forall(i <= _)),
          FormError(key, "error.invalid")
        )

      override def bind(
        key: String,
        data: Map[String, String]
      ): Either[Seq[FormError], LocalDate] = {
        val result = for {
          dateFieldStrings <- dateFieldStringValues(data)
          day ← toValidInt(dayKey, dateFieldStrings._1, Some(31))
          month ← toValidInt(monthKey, dateFieldStrings._2, Some(12))
          year ← toValidInt(yearKey, dateFieldStrings._3, None)
          date ← Either
                   .fromTry(Try(LocalDate.of(year, month, day)))
                   .leftMap(_ => FormError(dateKey, "error.invalid"))
                   .flatMap(date =>
                     if (minimumDateInclusive.exists(_.isAfter(date)))
                       Left(FormError(dateKey, "error.tooFarInPast"))
                     else if (date.isBefore(minimumDate))
                       Left(FormError(dateKey, "error.before1900"))
                     else Left(FormError(dateKey, "error.invalid"))
                   )
        } yield date

        result.leftMap(Seq(_))
      }

      override def unbind(key: String, value: LocalDate): Map[String, String] =
        Map(
          dayKey   -> value.getDayOfMonth.toString,
          monthKey -> value.getMonthValue.toString,
          yearKey  -> value.getYear.toString
        )

    }
}
