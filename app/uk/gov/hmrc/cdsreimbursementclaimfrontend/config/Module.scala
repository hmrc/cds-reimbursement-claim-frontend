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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.config

import com.google.inject.AbstractModule
import org.apache.pekko.actor.ActorSystem
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.Configuration
import play.api.Logger
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.client.HttpClientV2Impl
import uk.gov.hmrc.http.hooks.HookData.FromMap
import uk.gov.hmrc.http.hooks.HookData.FromString
import uk.gov.hmrc.http.hooks.Data
import uk.gov.hmrc.http.hooks.HttpHook
import uk.gov.hmrc.http.hooks.RequestData
import uk.gov.hmrc.http.hooks.ResponseData
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.HttpAuditing

import java.net.URL
import javax.inject.Inject
import javax.inject.Singleton

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.io.AnsiColor.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class Module extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[HttpClientV2]).to(classOf[DebuggingHttpClientV2])
    ()
  }
}

class DebuggingHook(config: Configuration) extends HttpHook {

  val shouldDebug: Boolean =
    config.underlying.getBoolean("outboundRequests.debug")

  override def apply(
    verb: String,
    url: URL,
    request: RequestData,
    responseF: Future[ResponseData]
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Unit = {
    if shouldDebug && !url.getPath().contains("/auth/authorise") then {
      responseF.andThen {
        case Success(response) =>
          Logger("OutboundRequest").debug(s"""$printRequest  
          |$YELLOW Response: $BOLD${response.status}$RESET
          |   ${response.headers.toSeq
                                              .flatMap { case (k, vs) => vs.map(v => s"$BLUE$k: $MAGENTA$v$RESET") }
                                              .mkString("\n   ")}
          |      
          |$GREEN${Try(Json.prettyPrint(Json.parse(response.body.value)))
                                              .getOrElse(response.body.value)}$RESET\n""".stripMargin)

        case Failure(exception) =>
          Logger("OutboundRequest").debug(
            s"""$printRequest
            |$RED_B$WHITE Failure: $BOLD${exception.toString()} $RESET
            |""".stripMargin
          )
      }
    }

    def printRequest =
      s"""
        | $BOLD$YELLOW$verb $CYAN$url$RESET 
        |   ${request.headers.map { case (k, v) => s"$BLUE$k: $MAGENTA$v$RESET" }.mkString("\n   ")}
        |
        |${request.body
          .map { case Data(value, _, _) =>
            value match {
              case FromMap(m) =>
                m.toSeq
                  .flatMap { case (k, vs) => vs.map(v => (k, v)) }
                  .map { case (k, v) => s"$k = $v" }
                  .mkString("\n   ")

              case FromString(s) =>
                s"$GREEN${Try(Json.prettyPrint(Json.parse(s))).getOrElse(s)}$RESET"
            }
          }
          .getOrElse("")}""".stripMargin
  }

}

@Singleton
class DebuggingHttpClientV2 @Inject() (
  config: Configuration,
  httpAuditing: HttpAuditing,
  wsClient: WSClient,
  actorSystem: ActorSystem
) extends HttpClientV2Impl(wsClient, actorSystem, config, Seq(httpAuditing.AuditingHook, new DebuggingHook(config)))
