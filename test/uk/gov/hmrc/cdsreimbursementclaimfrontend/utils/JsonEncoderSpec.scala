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

import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.node.JsonNodeFactory
import com.fasterxml.jackson.databind.node.TextNode

import scala.jdk.CollectionConverters._
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.core.OutputStreamAppender
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.io.OutputStream
import java.nio.ByteBuffer
import com.fasterxml.jackson.databind.JsonNode
import java.nio.charset.StandardCharsets

class JsonEncoderSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  val encoder = new JsonEncoder()
  val jnf     = JsonNodeFactory.instance

  "JsonEncoder" should {
    "decode empty message into a text node" in {
      val node = new ObjectNode(jnf)
      encoder.decodeMessage(node, "")
      node.get("message") shouldBe new TextNode("")
    }

    "decode non-json message into a text node" in {
      val node = new ObjectNode(jnf)
      encoder.decodeMessage(node, "foobar")
      node.get("message") shouldBe new TextNode("foobar")
    }

    "decode json message into an object node" in {
      val node = new ObjectNode(jnf)
      encoder.decodeMessage(node, """json{"foo":"bar"}""")
      node.get("cdsr")    shouldBe new ObjectNode(
        jnf,
        Map[String, JsonNode]("foo" -> new TextNode("bar")).asJava
      )
      node.get("message") shouldBe new TextNode("""{"foo":"bar"}""")
    }

    "fallback to decode json message into a text node" in {
      val node    = new ObjectNode(jnf)
      val message = """json{"foo":"bar}"""
      encoder.decodeMessage(node, message)
      node.get("message") shouldBe new TextNode(message)
    }

    "encode event without json message" ignore {
      assertLog("foo", """"message":"foo"""")
    }

    "encode event with json message" ignore {
      assertLog("""json{"foo":"bar"}""", """"cdsr":{"foo":"bar"}""")
    }

    def assertLog(message: String, expected: String) = {

      val context  = new LoggerContext()
      val appender = new OutputStreamAppender[ILoggingEvent]
      appender.setContext(context)
      val buf      = ByteBuffer.allocateDirect(1024)
      val out      = new OutputStream {
        override def write(b: Int): Unit = {
          println(b.toHexString)
          buf.put(b.toByte)
        }
      }
      appender.setOutputStream(out)
      val encoder  = new JsonEncoder()
      encoder.setContext(context)
      appender.setEncoder(encoder)
      appender.start()
      val logger   = context.getLogger("foo")
      logger.addAppender(appender)
      logger.info(message)
      buf.flip()
      val array    = Array.ofDim[Byte](buf.limit())
      buf.get(array)
      val log      = new String(array, StandardCharsets.UTF_8)
      println(log)
      log.contains(expected) shouldBe true
      buf.clear()
    }
  }

}
