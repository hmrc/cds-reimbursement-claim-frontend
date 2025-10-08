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

import scala.Iterable

/** Fluent syntax helper methods. */
trait FluentSyntax[Claim] {

  val claimEither: Either[String, Claim]

  /** Modify claim with function applied for each element of the collection. */
  final def mapEach[A](
    collection: Iterable[A],
    modifyFx: Claim => A => Claim
  ): Either[String, Claim] =
    collection.foldLeft(claimEither) { (result, item) =>
      result.map(claim => modifyFx(claim)(item))
    }

  /** Try to modify claim with function applied for each element of the collection. */
  final def flatMapEach[A](
    collection: Iterable[A],
    modifyFx: Claim => A => Either[String, Claim]
  ): Either[String, Claim] =
    collection.foldLeft(claimEither) { (result, item) =>
      result.flatMap(claim => modifyFx(claim)(item))
    }

  final def flatMapEachWhenDefined[A](option: Option[Iterable[A]])(
    modifyFx: Claim => A => Either[String, Claim]
  ): Either[String, Claim] =
    option match {
      case None             => claimEither
      case Some(collection) =>
        collection.foldLeft(claimEither) { (result, item) =>
          result.flatMap(claim => modifyFx(claim)(item))
        }
    }

  final def flatMapEachWhenDefinedAndMappingDefined[K, V](option: Option[Map[K, Option[V]]])(
    modifyFx: Claim => (K, V) => Either[String, Claim]
  ): Either[String, Claim] =
    option match {
      case None           => claimEither
      case Some(mappings) =>
        mappings.foldLeft(claimEither) {
          case (result, (key, Some(value))) =>
            result.flatMap(claim => modifyFx(claim)(key, value))

          case (result, _) => result
        }
    }

  final def flatMapEachWhenMappingDefined[K, V](mappings: Map[K, Option[V]])(
    modifyFx: Claim => (K, V) => Either[String, Claim]
  ): Either[String, Claim] =
    mappings.foldLeft(claimEither) {
      case (result, (key, Some(value))) =>
        result.flatMap(claim => modifyFx(claim)(key, value))

      case (result, _) => result
    }

  /** Try to modify the claim if the condition holds, otherwise return as is. */
  final def flatMapWhen(condition: Boolean)(
    modifyFx: Claim => Either[String, Claim]
  ): Either[String, Claim] =
    if condition then claimEither.flatMap(modifyFx) else claimEither

  /** Try to modify the claim if the condition holds, otherwise return as is. */
  final def flatMapWhen(condition: Claim => Boolean)(
    modifyFx: Claim => Either[String, Claim]
  ): Either[String, Claim] =
    claimEither
      .map(condition)
      .flatMap(flag => if flag then claimEither.flatMap(modifyFx) else claimEither)

  /** Try to modify the claim if the optional value is defined, otherwise return as is. */
  final def mapWhenDefined[A](option: Option[A])(
    modifyFx: Claim => A => Claim
  ): Either[String, Claim] =
    option match {
      case None        => claimEither
      case Some(value) => claimEither.map(modifyFx(_)(value))
    }

  /** Try to modify the claim if the optional value is defined, otherwise return as is. */
  final def flatMapWhenDefined[A](option: Option[A])(
    modifyFx: Claim => A => Either[String, Claim]
  ): Either[String, Claim] =
    option match {
      case None        => claimEither
      case Some(value) => claimEither.flatMap(modifyFx(_)(value))
    }

}

trait DirectFluentSyntax[Claim] extends FluentSyntax[Claim] {
  self: Claim =>

  val claimEither: Either[String, Claim] =
    Right(this)

}

/** Extension methods for Either[String, Claim].
  */
trait ImplicitFluentSyntax[Claim] {

  implicit class EitherClaimOps(val claimEither: Either[String, Claim]) extends FluentSyntax[Claim]

}
