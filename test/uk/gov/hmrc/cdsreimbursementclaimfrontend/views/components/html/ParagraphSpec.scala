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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.collection.JavaConverters._
import org.scalacheck.ShrinkLowPriority
import play.twirl.api.HtmlFormat

class ParagraphSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers with ShrinkLowPriority {

  val nonEmptyStringSeqGen =
    Gen
      .chooseNum(1, 100)
      .flatMap(n => Gen.sequence((1 to n).map(_ => Gen.asciiPrintableStr.suchThat(_.nonEmpty))))
      .map(_.asScala)

  "The Paragraph object" should {

    "take a single non-empty sstring and wrap into an html paragraph element" in {
      Paragraph("a").body shouldBe """
      |<p class="govuk-body">
      |    a
      |</p>
      |""".stripMargin
    }

    "take two non-empty strings and wrap into an html paragraph element" in {
      Paragraph("a", "b").body shouldBe """
      |<p class="govuk-body">
      |    a<br />b
      |</p>
      |""".stripMargin
    }

    "take three non-empty strings and wrap into an html paragraph element" in {
      Paragraph("abc", "123", "xyz").body shouldBe """
      |<p class="govuk-body">
      |    abc<br />123<br />xyz
      |</p>
      |""".stripMargin
    }

    "filter out empty strings" in {
      Paragraph("", "abc", "", "", "xyz", "").body shouldBe """
      |<p class="govuk-body">
      |    abc<br />xyz
      |</p>
      |""".stripMargin
    }

    "escape significant html characters" in {
      Paragraph("&", "<", "a", ">", "\"", "b").body shouldBe """
      |<p class="govuk-body">
      |    &amp;<br />&lt;<br />a<br />&gt;<br />&quot;<br />b
      |</p>
      |""".stripMargin
    }

    "concatenate any sequence of strings and wrap into an html paragraph element" in {
      forAll(nonEmptyStringSeqGen) { n =>
        Paragraph(n.head, n.tail: _*).body shouldBe s"""
        |<p class="govuk-body">
        |    ${n.map(HtmlFormat.escape).mkString("<br />")}
        |</p>
        |""".stripMargin
      }
    }
  }
}
