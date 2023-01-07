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

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Format
import play.api.libs.json.Json

trait JsonFormatTest {
  this: Matchers =>

  def validateJsonFormat[A](json: String, entity: A)(implicit format: Format[A]): Assertion = {
    val expectedJson = json.replaceAllLiterally("\n", "").replaceAllLiterally(" ", "")
    val actualJson   = Json.stringify(Json.toJson(entity))
    actualJson             shouldBe expectedJson
    Json.parse(json).as[A] shouldBe entity
  }

  def validateJsonReads[A](json: String, entity: A)(implicit format: Format[A]): Assertion =
    Json.parse(json).as[A] shouldBe entity

  def validateJsonWrites[A](json: String, entity: A)(implicit format: Format[A]): Assertion = {
    val expectedJson = json.replaceAllLiterally("\n", "").replaceAllLiterally(" ", "")
    val actualJson   = Json.stringify(Json.toJson(entity))
    actualJson shouldBe expectedJson
  }

  def validateCanReadAndWriteJson[A](entity: A)(implicit format: Format[A]): Assertion = {
    val actualJson = Json.stringify(Json.toJson(entity))
    actualJson                     should not be empty
    Json.parse(actualJson).as[A] shouldBe entity
  }

}
