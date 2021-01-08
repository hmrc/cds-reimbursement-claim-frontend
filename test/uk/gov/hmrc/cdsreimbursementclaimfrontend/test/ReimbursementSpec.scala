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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.test

import com.github.ghik.silencer.silent
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration, HttpConfiguration}
import play.api.i18n.{DefaultLangs, DefaultMessagesApiProvider, MessagesApi}
import play.api.mvc._
import play.api.test.Helpers._
import play.api.test.{FakeRequest, NoMaterializer}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AppConfig
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext

trait ReimbursementSpec extends AnyWordSpec with Matchers {

  @silent
  implicit val ec: ExecutionContext

  val env           = Environment.simple()
  val configuration = Configuration.load(env)
  val serviceConfig = new ServicesConfig(configuration)

  implicit val appConfig = new AppConfig(configuration, env, serviceConfig)

  val langs = new DefaultLangs()

  val messagesApiProvider = new DefaultMessagesApiProvider(
    environment = env,
    config = configuration,
    langs = langs,
    httpConfiguration = new HttpConfiguration()
  )

  val messagesApi: MessagesApi = messagesApiProvider.get

  implicit val mcc: MessagesControllerComponents =
    DefaultMessagesControllerComponents(
      messagesActionBuilder = new DefaultMessagesActionBuilderImpl(stubBodyParser(), messagesApi)(ec),
      actionBuilder = DefaultActionBuilder(stubBodyParser())(ec),
      parsers = stubPlayBodyParsers(NoMaterializer),
      messagesApi = messagesApi,
      langs = stubLangs(),
      fileMimeTypes = new DefaultFileMimeTypes(FileMimeTypesConfiguration(Map.empty)),
      executionContext = ec
    )

  val sessionId   = SessionId("session_1234")
  implicit val hc = HeaderCarrier(sessionId = Some(sessionId))

  val somePath = "/some/resource/path"
  val req      = FakeRequest("GET", somePath, Headers(("X-Session-ID", sessionId.value)), AnyContentAsEmpty)
}
