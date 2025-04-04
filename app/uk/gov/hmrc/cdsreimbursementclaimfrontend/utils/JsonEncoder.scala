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

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.spi.ThrowableProxyUtil
import ch.qos.logback.core.encoder.EncoderBase
import com.fasterxml.jackson.core.JsonGenerator.Feature
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import com.typesafe.config.ConfigFactory
import org.apache.commons.lang3.time.FastDateFormat
import play.api.Logger

import java.net.InetAddress
import java.nio.charset.StandardCharsets
import java.util.Locale

import scala.jdk.CollectionConverters.*
import scala.util.Success
import scala.util.Try
import scala.annotation.nowarn

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
@nowarn
class JsonEncoder extends EncoderBase[ILoggingEvent] {

  private val mapper = new ObjectMapper().configure(Feature.ESCAPE_NON_ASCII, true)

  lazy val appName: String = Try(ConfigFactory.load().getString("appName")) match {
    case Success(name) => name
    case _             => "APP NAME NOT SET"
  }

  private val DATE_FORMAT = "yyyy-MM-dd HH:mm:ss.SSSZZ"

  private lazy val dateFormat = {
    val dformat = Try(ConfigFactory.load().getString("logger.json.dateformat")) match {
      case Success(date) => date
      case _             => DATE_FORMAT
    }
    FastDateFormat.getInstance(dformat)
  }

  override def encode(event: ILoggingEvent): Array[Byte] = {
    val eventNode = mapper.createObjectNode

    eventNode.put("app", appName)
    eventNode.put("hostname", InetAddress.getLocalHost.getHostName)
    eventNode.put("timestamp", dateFormat.format(event.getTimeStamp))

    decodeMessage(eventNode, event.getFormattedMessage)

    Option(event.getThrowableProxy).map(p => eventNode.put("exception", ThrowableProxyUtil.asString(p)))

    eventNode.put("logger", event.getLoggerName)
    eventNode.put("thread", event.getThreadName)
    eventNode.put("level", event.getLevel.toString)

    Option(getContext).foreach(c =>
      c.getCopyOfPropertyMap.asScala.foreachEntry { case (k, v) =>
        eventNode.put(k.toLowerCase(Locale.ENGLISH), v)
      }
    )
    event.getMDCPropertyMap.asScala.foreachEntry { case (k, v) => eventNode.put(k.toLowerCase(Locale.ENGLISH), v) }

    s"${mapper.writeValueAsString(eventNode)}${System.lineSeparator()}".getBytes(StandardCharsets.UTF_8)
  }

  def decodeMessage(eventNode: ObjectNode, message: String): Unit =
    if message.startsWith("json{") then {
      eventNode.put("message", message.drop(4))
      try {
        val messageNode: JsonNode = mapper.readTree(message.drop(4))
        eventNode.set[JsonNode]("cdsr", messageNode)
      } catch {
        case e: Exception =>
          Logger(getClass()).error(s"${e.getMessage}\nmessage:$message")
          eventNode.put("message", message)
      }
    } else eventNode.put("message", message)

  override def footerBytes(): Array[Byte] =
    System.lineSeparator().getBytes(StandardCharsets.UTF_8)

  override def headerBytes(): Array[Byte] =
    System.lineSeparator().getBytes(StandardCharsets.UTF_8)

}
