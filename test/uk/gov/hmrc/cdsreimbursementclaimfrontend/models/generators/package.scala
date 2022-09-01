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

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.magnolia.Typeclass

import java.net.URL
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneId
import java.util.UUID

package object generators {

  def genStringWithMaxSizeOfN(max: Int): Gen[String] =
    Gen
      .choose(1, max)
      .flatMap(Gen.listOfN(_, Gen.alphaChar))
      .map(_.mkString(""))

  lazy val genLocalDateTime: Gen[LocalDateTime] =
    Gen
      .chooseNum(0L, 10000L)
      .map(millis => LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneId.systemDefault()))

  lazy val genBigDecimal: Gen[BigDecimal] =
    BigDecimalGen.amountNumberGen

  lazy val arbitraryBigDecimal: Arbitrary[BigDecimal] =
    BigDecimalGen.amountNumberArbitrary

  lazy val arbitraryBoolean: Typeclass[Boolean] = Arbitrary(
    Gen.oneOf(true, false)
  )

  implicit lazy val arbitraryString: Typeclass[String] = Arbitrary(
    Gen.nonEmptyListOf(Gen.alphaUpperChar).map(_.mkString(""))
  )

  implicit lazy val arbitraryLong: Arbitrary[Long] = Arbitrary(
    Gen.choose(-5e13.toLong, 5e13.toLong)
  )

  implicit lazy val arbitraryLocalDate: Arbitrary[LocalDate] = Arbitrary(
    Gen.chooseNum(0L, 10000L).map(LocalDate.ofEpochDay)
  )

  implicit lazy val arbitraryLocalDateTime: Arbitrary[LocalDateTime] =
    Arbitrary(genLocalDateTime)

  implicit lazy val arbitraryInstant: Arbitrary[Instant] =
    Arbitrary(
      Gen
        .chooseNum(0L, 10000L)
        .map(Instant.ofEpochMilli)
    )

  implicit lazy val arbitraryUuid: Arbitrary[UUID] = Arbitrary(UUID.randomUUID())

  lazy val genUrl: Gen[URL] =
    for {
      protocol <- Gen.oneOf("http", "https")
      hostname <- genStringWithMaxSizeOfN(7)
      domain   <- Gen.oneOf("com", "co.uk", "lv")
    } yield new URL(s"$protocol://$hostname.$domain")

  implicit lazy val arbitraryUrl: Arbitrary[URL] = Arbitrary(genUrl)
}
