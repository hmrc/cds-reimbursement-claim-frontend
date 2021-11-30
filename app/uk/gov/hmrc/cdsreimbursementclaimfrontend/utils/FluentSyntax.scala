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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

/** Fluent syntax helper methods. */
trait FluentSyntax[Journey] {
  this: Journey =>

  // Modify journey if the condition holds, otherwise return as is.
  def mapWhen(condition: => Boolean)(
    modifyFx: Journey => Journey
  ): Journey =
    if (condition) modifyFx(this) else this

  // Try to modify journey if the condition holds, otherwise return Right as is.
  def flatMapWhen(condition: => Boolean)(
    modifyFx: Journey => Either[String, Journey]
  ): Either[String, Journey] =
    if (condition) modifyFx(this) else Right(this)

  /** Try to modify the journey if the optional value is defined, otherwise return as is. */
  def mapWhenDefined[A](option: Option[A])(
    modifyFx: Journey => A => Journey
  ): Journey =
    option match {
      case None        => this
      case Some(value) => modifyFx(this)(value)
    }

  /** Try to modify the journey if the optional value is defined, otherwise return as is. */
  def flatMapWhenDefined[A](option: Option[A])(
    modifyFx: Journey => A => Either[String, Journey]
  ): Either[String, Journey] =
    option match {
      case None        => Right(this)
      case Some(value) => modifyFx(this)(value)
    }

}

/** Extension methods for Either[String, Journey].
  */
trait FluentImplicits[Journey] {

  implicit class EitherJourneyOps(val journeyEither: Either[String, Journey]) {

    /** Modify journey with function applied for each element of the collection. */
    def mapEach[A](
      collection: Traversable[A],
      modifyFx: Journey => A => Journey
    ): Either[String, Journey] =
      collection.foldLeft(journeyEither) { (result, item) =>
        result.map(journey => modifyFx(journey)(item))
      }

    /** Try to modify journey with function applied for each element of the collection. */
    def flatMapEach[A](
      collection: Traversable[A],
      modifyFx: Journey => A => Either[String, Journey]
    ): Either[String, Journey] =
      collection.foldLeft(journeyEither) { (result, item) =>
        result.flatMap(journey => modifyFx(journey)(item))
      }

    /** Try to modify the journey if the condition holds, otherwise return as is. */
    def flatMapWhen(condition: Boolean)(
      modifyFx: Journey => Either[String, Journey]
    ): Either[String, Journey] =
      if (condition) journeyEither.flatMap(modifyFx) else journeyEither

    /** Try to modify the journey if the condition holds, otherwise return as is. */
    def flatMapWhen(condition: Journey => Boolean)(
      modifyFx: Journey => Either[String, Journey]
    ): Either[String, Journey] =
      journeyEither
        .map(condition)
        .flatMap(flag => if (flag) journeyEither.flatMap(modifyFx) else journeyEither)

    /** Try to modify the journey if the optional value is defined, otherwise return as is. */
    def mapWhenDefined[A](option: Option[A])(
      modifyFx: Journey => A => Journey
    ): Either[String, Journey] =
      option match {
        case None        => journeyEither
        case Some(value) => journeyEither.map(modifyFx(_)(value))
      }

    /** Try to modify the journey if the optional value is defined, otherwise return as is. */
    def flatMapWhenDefined[A](option: Option[A])(
      modifyFx: Journey => A => Either[String, Journey]
    ): Either[String, Journey] =
      option match {
        case None        => journeyEither
        case Some(value) => journeyEither.flatMap(modifyFx(_)(value))
      }

  }

}
