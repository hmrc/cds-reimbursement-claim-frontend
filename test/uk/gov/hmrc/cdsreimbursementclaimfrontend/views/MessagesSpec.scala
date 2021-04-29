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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.http.HttpConfiguration
import play.api.i18n.{DefaultLangs, DefaultMessagesApiProvider}
import play.api.{Configuration, Environment}
import play.i18n.Lang

class MessagesSpec extends AnyWordSpec with Matchers {

  val env           = Environment.simple()
  val configuration = Configuration.load(env)
  val defaultLangs  = new DefaultLangs(Seq(Lang.forCode("en"), Lang.forCode("cy")))

  val messageApi = new DefaultMessagesApiProvider(env, configuration, defaultLangs, HttpConfiguration()).get

  protected val messagesEn = messageApi.messages.get("default").getOrElse(fail("English messages was not found"))
  protected val messagesCy = messageApi.messages.get("cy").getOrElse(fail("Welsh messages was not found"))

  "Messages" should {

    "Show language keys and translations are missing in welsh" in {
      val missingLinesFromWelsh = messagesEn.toList
        .map(englishKV => if (messagesCy.isDefinedAt(englishKV._1)) None else Some(englishKV))
        .flatten(Option.option2Iterable)
        .sortWith((kv1, kv2) => kv1._1 < kv2._1)
        .map(kv => kv._1 + "=" + kv._2)

      if (missingLinesFromWelsh.length > 0) {
        println(System.lineSeparator() * 2)
        println("=" * 100)
        println(
          "=" * 15 + s"   Missing Language keys (and values) from messages.cy, counted: ${missingLinesFromWelsh.length}  " + "=" * 15
        )
        println("=" * 100)
        println(missingLinesFromWelsh.mkString(System.lineSeparator()))
      }
    }

    "Show orphaned welsh translations, for which the keys no longer exist in english" in {
      val orphanedWelshLines = messagesCy.toList
        .map(englishKV => if (messagesEn.isDefinedAt(englishKV._1)) None else Some(englishKV))
        .flatten(Option.option2Iterable)
        .sortWith((kv1, kv2) => kv1._1 < kv2._1)
        .map(kv => kv._1 + "=" + kv._2)

      if (orphanedWelshLines.length > 0) {
        println(System.lineSeparator() * 2)
        println("=" * 110)
        println(
          "=" * 5 + s"   Orphaned welsh translations, for which the keys no longer exist in english: ${orphanedWelshLines.length} (Delete these)  " + "=" * 5
        )
        println("=" * 110)
        println(orphanedWelshLines.mkString(System.lineSeparator()))
      }
    }

    "Show untranslated welsh lines" in {
      val excludedKeys = List("footer.", "language-switcher.", "country.", "currency", "header.govuk.url", "lang")

      val untranslatedInWelsh = messagesCy.toList
        .filter { welshKV =>
          messagesEn.get(welshKV._1) match {
            case Some(english) =>
              if (english === welshKV._2 || english === "___" + welshKV._2) true else false
            case None          => false
          }
        }
        .filterNot(kv => excludedKeys.filter(a => kv._1.startsWith(a)).nonEmpty)
        .sortWith((kv1, kv2) => kv1._1 < kv2._1)
        .map(kv => kv._1 + "=" + kv._2)

      if (untranslatedInWelsh.length > 0) {
        println(System.lineSeparator() * 2)
        println("=" * 100)
        println(
          "=" * 15 + s"   Untranslated/English text in the Welsh messages.cy, counted: ${untranslatedInWelsh.length}  " + "=" * 15
        )
        println("=" * 100)
        println(untranslatedInWelsh.mkString(System.lineSeparator()))
      }
    }

  }
}
