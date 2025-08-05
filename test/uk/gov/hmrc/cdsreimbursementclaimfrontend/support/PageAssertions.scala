/*
 * Copyright 2025 HM Revenue & Customs
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

import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import org.scalactic.source.Position
import org.scalatest.matchers.should.Matchers
import scala.jdk.CollectionConverters.*

trait PageAssertions {
  self: Matchers =>

  final def isCheckboxChecked(document: Document, fieldValue: String): Boolean =
    document.select(s"""input[value="$fieldValue"] """).hasAttr("checked")

  final def isChecked(document: Document, fieldId: String): Boolean =
    Option(document.getElementById(fieldId))
      .orElse {
        val es = document.getElementsByAttributeValue("data-id", fieldId)
        if es.size() > 0 then Some(es.first()) else None
      }
      .map(
        _.attributes()
          .asList()
          .asScala
          .map(_.getKey)
          .contains("checked")
      )
      .getOrElse(false)

  import cats.instances.int._
  import cats.syntax.eq._

  /** Returns sequence of pairs (message, value) */
  final def radioItems(doc: Document): Seq[(String, String)] = {
    val labels = doc.select("div.govuk-radios label").eachText()
    val values = doc.select("div.govuk-radios input").eachAttr("value")
    labels.asScala.zip(values.asScala).toSeq
  }

  /** Returns sequence of pairs (message, value) */
  final def checkboxes(doc: Document): Seq[(String, String)] = {
    val labels = doc.select("div.govuk-checkboxes label").eachText()
    val values = doc.select("div.govuk-checkboxes input").eachAttr("value")
    labels.asScala.zip(values.asScala).toSeq
  }

  final def checkboxesWithHints(doc: Document): Seq[(String, String)] = {
    val labels = doc.select("div.govuk-checkboxes label").eachText()
    val hints  = doc.select("div.govuk-checkboxes div.govuk-hint").eachText()
    labels.asScala.zip(hints.asScala).toList
  }

  final def selectedRadioValue(doc: Document): Option[String] = {
    val radioItems = doc.select("div.govuk-radios input[checked]")
    if radioItems.size() =!= 0 then Some(radioItems.`val`())
    else None
  }

  final def selectedTextArea(doc: Document): Option[String] = {
    val textArea = doc.select("textarea.govuk-textarea")
    if textArea.size() =!= 0 then Some(textArea.`val`()) else None
  }

  final def selectedCheckBox(doc: Document): Seq[String] = {
    val checkBoxes: Elements = doc.select("div.govuk-checkboxes input[checked]")
    checkBoxes.eachAttr("value").asScala.toSeq
  }

  final def selectedInputBox(doc: Document, inputName: String): Option[String] = {
    val inputString: String = s"input.govuk-input[name='$inputName']"
    val input               = doc.select(inputString)
    if input.size() =!= 0 then Some(input.`val`()) else None
  }

  final def selectedInput(doc: Document): Option[String] = {
    val input = doc.select("input.govuk-input[checked]")
    if input.size() =!= 0 then Some(input.`val`()) else None
  }

  final def summaryKeyValue(doc: Document): (Seq[String], Seq[String]) = {
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    (summaryKeys.asScala.toSeq, summaryValues.asScala.toSeq)
  }

  final def summaryKeyValueList(doc: Element): Seq[(String, String)] = {
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    summaryKeys.asScala.zip(summaryValues.asScala).toSeq
  }

  final def summaryKeyValueMap(doc: Document): Map[String, String] = {
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    summaryKeys.asScala.zip(summaryValues.asScala).toMap
  }

  final def hasContinueButton(doc: Document)(implicit pos: Position) =
    if doc.select("button.govuk-button").eachText.asScala.exists(_ == "Continue") then succeed
    else fail("Expected page to have [Continue] button but none found")

  final def formAction(doc: Document) =
    doc.select("form").attr("action")

  final def assertThatNoneRadioButtonIsSelected: Document => Any =
    (doc: Document) => selectedRadioValue(doc).shouldBe(None)

  final def assertThatRadioButtonIsSelected(expected: String): Document => Any =
    (doc: Document) => selectedRadioValue(doc).shouldBe(Some(expected))

  final def assertPageElementsByIdAndExpectedText(doc: Document)(idsWithExpectedContentMap: (String, String)*): Any =
    idsWithExpectedContentMap.foreach { case (elementId, expectedText) =>
      val element = doc.getElementById(elementId)
      if element == null then {
        fail(s"""Missing page element with id="$elementId"""")
      } else {
        withClue(s"Inside ${element.outerHtml()} has text ")(
          element.text() shouldBe expectedText
        )
      }
    }

  final def assertPageElementsByIdAndExpectedHtml(doc: Document)(idsWithExpectedContentMap: (String, String)*): Any =
    idsWithExpectedContentMap.foreach { case (elementId, expectedHtml) =>
      val element = doc.getElementById(elementId)
      if element == null then {
        fail(s"""Missing page element with id="$elementId"""")
      } else {
        withClue(s"Inside ${element.outerHtml()} has text ")(
          element.html shouldBe expectedHtml
        )
      }
    }

  final def assertPageElementsBySelectorAndExpectedText(doc: Document)(
    selectorsWithExpectedContentMap: (String, String)*
  ): Any =
    selectorsWithExpectedContentMap.foreach { case (selector, expectedText) =>
      val elements = doc.select(selector)
      if elements == null || elements.isEmpty() then {
        fail(s"""Missing page element selected by $selector""")
      } else {
        elements.asScala.foreach(e => withClue(s"Inside ${e.outerHtml()} has text ")(e.text() shouldBe expectedText))
      }
    }

  final def assertPageInputsByIdAndExpectedValue(doc: Document)(idsWithExpectedContentMap: (String, String)*): Any =
    idsWithExpectedContentMap.foreach { case (elementId, expectedValue) =>
      val element = doc.getElementById(elementId)
      if element == null then {
        fail(s"""Missing page element with id="$elementId"""")
      } else {
        withClue(s"Inside ${element.outerHtml()} has value ")(
          element.`val`() shouldBe expectedValue
        )
      }
    }

  final def assertShowsInputError(doc: Document, errorMessage: Option[String] = None): Any = {
    val summaryError =
      Option(doc.select(".govuk-error-summary__list > li:nth-child(1) > a")).map(_.text())
    val inputError   =
      Option(doc.select("p.govuk-error-message").first()).map(_.ownText())

    withClue("when asserting errors on page") {
      summaryError shouldBe defined
      inputError   shouldBe defined
      errorMessage.foreach { m =>
        summaryError.get shouldBe m
        inputError.get   shouldBe m
      }
    }
  }
}
