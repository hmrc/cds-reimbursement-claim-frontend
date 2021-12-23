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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object Generators {

  def alphaNumGenerator(n: Int): Gen[String] = Gen.listOfN(n, Gen.alphaNumChar).map(_.mkString)
  def alphaNumGen(n: Int): String            = alphaNumGenerator(n).sample.getOrElse(sys.error(s"Could not generate instance"))

  def alphaCharGen(n: Int): String =
    Gen.listOfN(n, Gen.alphaChar).map(_.mkString).sample.getOrElse(sys.error(s"Could not generate instance"))

  def numStringGen(n: Int): String =
    Gen.listOfN(n, Gen.numChar).map(_.mkString).sample.getOrElse(sys.error(s"Could not generate instance"))

  def moneyGen(integralPart: Int, fractionalPart: Int): String = {
    val finalIntegral   = integralPart match {
      case 0      => ""
      case 1      => "9"
      case s: Int => s"9${numStringGen(s - 1)}"
    }
    val finalFractional = fractionalPart match {
      case 0      => ""
      case 1      => "9"
      case s: Int => s"${numStringGen(s - 1)}9"
    }
    finalIntegral + "." + finalFractional
  }

  def sample[A](implicit anItem: Arbitrary[A]): A =
    sample(anItem.arbitrary)

  def sample[A](gen: Gen[A]): A =
    gen.sample.getOrElse(sys.error(s"Could not generate instance with $gen"))

  def genOtherThan[T](t: T)(implicit gen: Arbitrary[T]): T =
    gen.arbitrary.suchThat(_ != t).sample.getOrElse(sys.error(s"Could not generate instance"))
}
