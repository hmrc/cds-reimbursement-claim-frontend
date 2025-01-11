/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.compiletime.*
import scala.deriving.Mirror

object GeneratorUtils {

  inline def gen[T](using m: Mirror.Of[T]): Arbitrary[T] =
    inline m match {
      case p: Mirror.ProductOf[T] =>
        Arbitrary(
          summonGeneratorOfTuple[T, p.MirroredElemTypes]
            .asInstanceOf[Gen[p.MirroredElemTypes]]
            .map(tup => p.fromTuple(tup))
        )

      case s: Mirror.SumOf[T] =>
        val gens: List[Gen[T]] = summonListOfGenerators[T, s.MirroredElemTypes]
        Arbitrary(
          Gen.choose(0, gens.size - 1).flatMap(i => gens(i))
        )
    }

  inline def summonGeneratorOfTuple[T, Elems <: Tuple]: Gen[Tuple] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems) =>
        summonOrDerive[T, elem].arbitrary
          .flatMap(head => summonGeneratorOfTuple[T, elems].map(tail => head *: tail))

      case _: EmptyTuple => Gen.const(EmptyTuple)
    }

  inline def summonListOfGenerators[T, Elems <: Tuple]: List[Gen[T]] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems) =>
        summonOrDerive[T, elem].arbitrary.asInstanceOf[Gen[T]] :: summonListOfGenerators[T, elems]

      case _: EmptyTuple =>
        Nil
    }

  inline def summonOrDerive[T, Elem]: Arbitrary[Elem] =
    summonFrom {
      case a: Arbitrary[Elem] => a
      case _                  =>
        inline erasedValue[T] match {
          case _: Elem => error("infinite recursive derivation")
          case _       => gen[Elem](using summonInline[Mirror.Of[Elem]]) // recursive derivation}
        }
    }
}
