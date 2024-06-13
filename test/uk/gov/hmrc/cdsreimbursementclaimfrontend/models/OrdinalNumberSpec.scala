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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.i18n._

class OrdinalNumberSpec extends AnyWordSpec with Matchers {

  val lang = Lang("foo")

  "OrdinalNumber" should {
    "render just number if no label nor suffix defined" in {
      implicit val m: Messages = parseMessages("")
      OrdinalNumber(1)  shouldBe "1"
      OrdinalNumber(13) shouldBe "13"
    }
    "render label if defined" in {
      implicit val m: Messages = parseMessages("ordinal.label.13=thirteen")
      OrdinalNumber(1)  shouldBe "1"
      OrdinalNumber(13) shouldBe "thirteen"
    }
    "render number with suffix if defined" in {
      implicit val m: Messages = parseMessages(
        """|ordinal.suffix.1=st
            |ordinal.suffix.3=rd
            |ordinal.suffix.5=th
            |ordinal.suffix.1x=th""".stripMargin
      )
      OrdinalNumber(1)   shouldBe "1st"
      OrdinalNumber(2)   shouldBe "2"
      OrdinalNumber(3)   shouldBe "3rd"
      OrdinalNumber(4)   shouldBe "4"
      OrdinalNumber(5)   shouldBe "5th"
      OrdinalNumber(6)   shouldBe "6"
      OrdinalNumber(7)   shouldBe "7"
      OrdinalNumber(8)   shouldBe "8"
      OrdinalNumber(9)   shouldBe "9"
      OrdinalNumber(10)  shouldBe "10th"
      OrdinalNumber(11)  shouldBe "11th"
      OrdinalNumber(12)  shouldBe "12th"
      OrdinalNumber(13)  shouldBe "13th"
      OrdinalNumber(14)  shouldBe "14th"
      OrdinalNumber(15)  shouldBe "15th"
      OrdinalNumber(16)  shouldBe "16th"
      OrdinalNumber(17)  shouldBe "17th"
      OrdinalNumber(18)  shouldBe "18th"
      OrdinalNumber(19)  shouldBe "19th"
      OrdinalNumber(20)  shouldBe "20"
      OrdinalNumber(21)  shouldBe "21st"
      OrdinalNumber(22)  shouldBe "22"
      OrdinalNumber(31)  shouldBe "31st"
      OrdinalNumber(101) shouldBe "101st"
    }
    "prefer label over suffix" in {
      implicit val m: Messages = parseMessages(
        """|ordinal.suffix.1=st
            |ordinal.label.1=first
            |ordinal.suffix.1x=th
            |ordinal.label.13=thirteen""".stripMargin
      )
      OrdinalNumber(1)  shouldBe "first"
      OrdinalNumber(2)  shouldBe "2"
      OrdinalNumber(12) shouldBe "12th"
      OrdinalNumber(13) shouldBe "thirteen"
      OrdinalNumber(14) shouldBe "14th"
    }
    "render english ordinal numerals" in {
      implicit val m: Messages = parseMessagesFile("/messages")
      OrdinalNumber(0)  shouldBe "zero"
      OrdinalNumber(1)  shouldBe "first"
      OrdinalNumber(2)  shouldBe "second"
      OrdinalNumber(3)  shouldBe "third"
      OrdinalNumber(4)  shouldBe "fourth"
      OrdinalNumber(5)  shouldBe "fifth"
      OrdinalNumber(6)  shouldBe "sixth"
      OrdinalNumber(7)  shouldBe "seventh"
      OrdinalNumber(8)  shouldBe "eighth"
      OrdinalNumber(9)  shouldBe "ninth"
      OrdinalNumber(10) shouldBe "10th"
      OrdinalNumber(11) shouldBe "11th"
      OrdinalNumber(12) shouldBe "12th"
      OrdinalNumber(13) shouldBe "13th"
      OrdinalNumber(14) shouldBe "14th"
      OrdinalNumber(15) shouldBe "15th"
      OrdinalNumber(16) shouldBe "16th"
      OrdinalNumber(17) shouldBe "17th"
      OrdinalNumber(18) shouldBe "18th"
      OrdinalNumber(19) shouldBe "19th"
      OrdinalNumber(20) shouldBe "20th"
      OrdinalNumber(21) shouldBe "21st"
      OrdinalNumber(22) shouldBe "22nd"
      OrdinalNumber(23) shouldBe "23rd"
      OrdinalNumber(24) shouldBe "24th"
      OrdinalNumber(25) shouldBe "25th"
      OrdinalNumber(26) shouldBe "26th"
      OrdinalNumber(27) shouldBe "27th"
      OrdinalNumber(28) shouldBe "28th"
      OrdinalNumber(29) shouldBe "29th"
      OrdinalNumber(30) shouldBe "30th"
    }
  }

  def parseMessages(string: String): Messages =
    MessagesImpl(
      lang = lang,
      messagesApi = new DefaultMessagesApi(
        messages = Map(
          lang.code -> Messages
            .parse(
              new Messages.MessageSource {
                override def read: String = string
              },
              ""
            )
            .getOrElse(fail())
        ),
        langs = new DefaultLangs(Seq(lang))
      )
    )

  def parseMessagesFile(path: String): Messages =
    MessagesImpl(
      lang = Lang("en"),
      messagesApi = new DefaultMessagesApi(
        messages = Map(
          "en" -> Messages
            .parse(
              Messages.UrlMessageSource(getClass.getResource(path)),
              ""
            )
            .getOrElse(fail())
        ),
        langs = new DefaultLangs(Seq(Lang("en")))
      )
    )
}
