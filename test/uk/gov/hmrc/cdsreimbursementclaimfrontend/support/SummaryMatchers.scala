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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.support

import cats.Eq
import cats.syntax.eq.*
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

trait SummaryMatchers {

  final def containAllDefinedElementsOf[A : Eq](sequence: Option[A]*): Matcher[Seq[A]] = {
    val expected: Seq[A] = sequence.collect { case Some(x) => x }
    new Matcher[Seq[A]] {
      override def apply(obtained: Seq[A]): MatchResult = {
        val missing: Seq[A]    = expected.filterNot(item => obtained.exists(_ === item))
        val unexpected: Seq[A] = obtained.filterNot(item => expected.exists(_ === item)).distinct
        if missing.isEmpty then MatchResult(true, "", "")
        else {
          MatchResult(
            false,
            s"Some expected elements are missing: \n- ${missing
                .mkString("\n- ")}${
                if unexpected.nonEmpty then s"\nThere are unexpected elements:\n- ${unexpected.mkString("\n- ")}"
                else ""
              }",
            ""
          )
        }
      }
    }
  }

  final def containOnlyDefinedElementsOf[A : Eq](sequence: Option[A]*): Matcher[Seq[A]] = {
    val expected: Seq[A] = sequence.collect { case Some(x) => x }
    new Matcher[Seq[A]] {
      override def apply(obtained: Seq[A]): MatchResult = {
        val missing: Seq[A]    = expected.filterNot(item => obtained.exists(_ === item))
        val unexpected: Seq[A] = obtained.filterNot(item => expected.exists(_ === item)).distinct
        if missing.isEmpty && unexpected.isEmpty then MatchResult(true, "", "")
        else {
          MatchResult(
            false,
            s"${
                if missing.nonEmpty then s"\nSome expected elements are missing: \n- ${missing.mkString("\n- ")}"
                else ""
              }${
                if unexpected.nonEmpty then s"\nSome elements are unexpected:\n- ${unexpected.mkString("\n- ")}"
                else ""
              }",
            ""
          )
        }
      }
    }
  }

  final def containAllDefinedPairsOf[A : Eq](sequence: Seq[(String, Option[A])]): Matcher[Seq[(String, A)]] = {
    val expected: Seq[(String, A)] = sequence.collect { case (n, Some(x)) => (n, x) }
    containAllPairsOf(expected)
  }

  final def containOnlyDefinedPairsOf[A : Eq](sequence: Seq[(String, Option[A])]): Matcher[Seq[(String, A)]] = {
    val expected: Seq[(String, A)] = sequence.collect { case (n, Some(x)) => (n, x) }
    containOnlyPairsOf(expected)
  }

  final def containAllPairsOf[A : Eq](expected: Seq[(String, A)]): Matcher[Seq[(String, A)]] =
    new Matcher[Seq[(String, A)]] {
      override def apply(obtained: Seq[(String, A)]): MatchResult = {
        val missingKeys: Seq[String] =
          expected.filterNot(item => obtained.exists(_._1 === item._1)).map(_._1)

        val unexpectedKeys: Seq[String] =
          obtained.filterNot(item => expected.exists(_._1 === item._1)).map(_._1).distinct

        val invalidMappings: Seq[(String, A, A)] =
          expected
            .flatMap(item =>
              if obtained.exists(_ === item) then Seq.empty
              else
                obtained.collect {
                  case (key, value) if key === item._1 && item._2 =!= value =>
                    (key, item._2, value)
                }
            )

        if missingKeys.isEmpty && invalidMappings.isEmpty then MatchResult(true, "", "")
        else {
          val missingKeysMessage   =
            if missingKeys.nonEmpty then
              s"\nSome expected keys are missing:\n- ${missingKeys
                  .mkString("\n- ")}${
                  if unexpectedKeys.nonEmpty then s"\nThere are unexpected keys:\n- ${unexpectedKeys.mkString("\n- ")}"
                  else ""
                }"
            else ""
          val invalidValuesMessage =
            if invalidMappings.nonEmpty then {
              val invalidMappingsMessage = invalidMappings
                .map { case (key, expected, obtained) =>
                  s"- $key: got [$obtained] but [$expected] was expected"
                }
                .mkString("\n")
              s"${if missingKeys.nonEmpty then "\n" else ""}Some values are invalid:\n$invalidMappingsMessage"
            } else ""
          MatchResult(
            false,
            missingKeysMessage + invalidValuesMessage,
            ""
          )
        }
      }
    }

  final def containOnlyPairsOf[A : Eq](expected: Seq[(String, A)]): Matcher[Seq[(String, A)]] =
    new Matcher[Seq[(String, A)]] {
      override def apply(obtained: Seq[(String, A)]): MatchResult = {
        val missingKeys: Seq[String] =
          expected.filterNot(item => obtained.exists(_._1 === item._1)).map(_._1)

        val unexpectedKeys: Seq[String] =
          obtained.filterNot(item => expected.exists(_._1 === item._1)).map(_._1).distinct

        val invalidMappings: Seq[(String, A, A)] =
          expected
            .flatMap(item =>
              if obtained.exists(_ === item) then Seq.empty
              else
                obtained.collect {
                  case (key, value) if key === item._1 && item._2 =!= value =>
                    (key, item._2, value)
                }
            )

        if missingKeys.isEmpty && unexpectedKeys.isEmpty && invalidMappings.isEmpty then MatchResult(true, "", "")
        else {
          val keysMessage          =
            s"${
                if missingKeys.nonEmpty then
                  s"\nSome expected keys are missing:\n- ${missingKeys
                      .mkString("\n- ")}"
                else ""
              }${
                if unexpectedKeys.nonEmpty then s"\nThere are unexpected keys:\n- ${unexpectedKeys.mkString("\n- ")}"
                else ""
              }"
          val invalidValuesMessage =
            if invalidMappings.nonEmpty then {
              val invalidMappingsMessage = invalidMappings
                .map { case (key, expected, obtained) =>
                  s"- $key: got [$obtained] but [$expected] was expected"
                }
                .mkString("\n")
              s"${if missingKeys.nonEmpty || unexpectedKeys.nonEmpty then "\n" else ""}Some values are invalid:\n$invalidMappingsMessage"
            } else ""
          MatchResult(
            false,
            keysMessage + invalidValuesMessage,
            ""
          )
        }
      }
    }

  implicit class OptionalOps[A](val value: A) {
    def expectedAlways: Option[A]               = Some(value)
    def expectedWhen(test: Boolean): Option[A]  = if test then Some(value) else None
    def expectedWhen(opt: Option[_]): Option[A] = if opt.isDefined then Some(value) else None
  }

  implicit class OptionalPairValueOps[A](val value: (String, Option[A])) {
    def expectedWhen(test: Boolean): (String, Option[A])  = if test then value else (value._1, None)
    def expectedWhen(opt: Option[_]): (String, Option[A]) = if opt.isDefined then value else (value._1, None)
  }

  implicit class PairValueOps[A](val value: (String, A)) {
    def expectedWhen(test: Boolean): (String, Option[A])  = if test then (value._1, Some(value._2)) else (value._1, None)
    def expectedWhen(opt: Option[_]): (String, Option[A]) =
      if opt.isDefined then (value._1, Some(value._2)) else (value._1, None)
  }

}
