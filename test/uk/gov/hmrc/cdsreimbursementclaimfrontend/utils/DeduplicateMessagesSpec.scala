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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.io.Codec
import scala.io.Source
import play.utils.Colors

class DeduplicateMessagesSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  lazy val lines = Source
    .fromResource("messages")(Codec.UTF8)
    .getLines()
    .map(_.trim())
    .filterNot(_.isEmpty())
    .filterNot(_.startsWith("#"))
    .toSeq

  "Messages" should {
    "not have duplicated keys" in {
      val duplicatedKeys = lines.map(_.split("=")).groupBy(_.head).filter(_._2.length > 1)
      if (duplicatedKeys.size == 0) {
        succeed
      } else {
        println(
          duplicatedKeys.map { case (k, v) => s"Key ${Colors.magenta(k)} has ${v.length} duplicates" }.mkString("\n")
        )
        fail()
      }
    }

    "not have duplicated messages" in {
      val duplicatedMessages =
        lines
          .map(_.split("="))
          .map(a => (a.head, a.tail.mkString("=")))
          .groupBy(_._2)
          .view
          .filterKeys(_.nonEmpty)
          .mapValues(
            _.map(_._1).filter(s => !s.startsWith("check-your-answers.") && !duplicatedMessagesAllowList.contains(s))
          )
          .filter(_._2.length > 1)
          .toSeq

      if (duplicatedMessages.size == 0) {
        succeed
      } else {
        println(s"Found ${duplicatedMessages.size} duplicated messages.")
        println(
          duplicatedMessages.zipWithIndex
            .map { case ((value, keys), index) =>
              s"${index + 1}. Keys:\n${keys.map(Colors.magenta).mkString("\n")}\nhave the same value ${Colors.cyan(value)}"
            }
            .mkString("\n")
        )
      }
    }
  }

  val duplicatedMessagesAllowList =
    """service.name
|service.title
|landing.title
|landing.before.heading
|landing-securities.before.heading
|landing.start-button
|landing-securities.start-button
|invalid-reason-for-security.submit
|declaration-not-found.submit
|claimant-details.contact.title
|claimant-details.contact.details
|check-claimant-details.contact.details
|check-claimant-details.change-hint.contact
|select-securities.title.caption
|confirm-full-repayment.caption""".stripMargin.linesIterator.toSet

}
