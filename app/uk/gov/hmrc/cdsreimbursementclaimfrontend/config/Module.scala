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

import org.apache.pekko.actor.ActorSystem
import com.google.inject.AbstractModule
import com.hhandoko.play.pdf.PdfGenerator
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.Configuration
import play.api.Environment
import play.api.Logger
import uk.gov.hmrc.http.hooks.HookData.FromMap
import uk.gov.hmrc.http.hooks.HookData.FromString
import uk.gov.hmrc.http.hooks.Data
import uk.gov.hmrc.http.hooks.HttpHook
import uk.gov.hmrc.http.hooks.RequestData
import uk.gov.hmrc.http.hooks.ResponseData
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.play.audit.http.HttpAuditing
import uk.gov.hmrc.play.bootstrap.http.DefaultHttpClient

import java.net.URL
import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.io.AnsiColor._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class Module(environment: Environment, configuration: Configuration) extends AbstractModule {

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def configure(): Unit = {
    bind(classOf[HttpClient]).to(classOf[DebuggingHttpClient])
    bind(classOf[PdfGenerator]).toInstance(new PdfGenerator(environment))
    ()
  }
}

@Singleton
class DebuggingHttpClient @Inject() (
  config: Configuration,
  override val httpAuditing: HttpAuditing,
  override val wsClient: WSClient,
  override protected val actorSystem: ActorSystem
) extends DefaultHttpClient(config, httpAuditing, wsClient, actorSystem) {

  override val hooks: Seq[HttpHook] = Seq(httpAuditing.AuditingHook, new DebuggingHook(config))
}

class DebuggingHook(config: Configuration) extends HttpHook {

  val shouldDebug: Boolean =
    config.underlying.getBoolean("outboundRequests.debug")

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def apply(
    verb: String,
    url: URL,
    request: RequestData,
    responseF: Future[ResponseData]
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Unit = {
    if (shouldDebug && !url.getPath().contains("/auth/authorise")) {
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
