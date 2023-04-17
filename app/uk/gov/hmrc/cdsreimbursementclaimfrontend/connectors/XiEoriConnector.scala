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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import akka.actor.ActorSystem
import cats.syntax.eq._
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import play.api.Configuration
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Singleton
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DefaultXiEoriConnector])
trait XiEoriConnector {

  def getXiEori(implicit hc: HeaderCarrier): Future[UserXiEori]
}

object XiEoriConnector {

  final case class Response(eoriGB: String, eoriXI: String)
  final case class Exception(msg: String) extends scala.RuntimeException(msg)

  implicit val format: Format[Response] = Json.format[Response]
}

@Singleton
class DefaultXiEoriConnector @Inject() (
  http: HttpClient,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  val actorSystem: ActorSystem
)(implicit
  ec: ExecutionContext
) extends XiEoriConnector
    with Retries
    with Logging {

  lazy val baseUrl: String = servicesConfig.baseUrl("cds-reimbursement-claim")

  lazy val contextPath: String =
    servicesConfig.getConfString("cds-reimbursement-claim.context-path", "cds-reimbursement-claim")

  lazy val url = s"$baseUrl$contextPath/eori/xi"

  lazy val retryIntervals: Seq[FiniteDuration] =
    Retries.getConfIntervals("cds-reimbursement-claim", configuration)

  override def getXiEori(implicit hc: HeaderCarrier): Future[UserXiEori] =
    retry(retryIntervals: _*)(shouldRetry, retryReason)(
      http.GET[HttpResponse](url)
    ).flatMap {
      case response if response.status === 200 =>
        Future(response.json.as[XiEoriConnector.Response])
          .transform(r => UserXiEori(r.eoriXI), e => new XiEoriConnector.Exception(e.getMessage))

      case response if response.status === 204 =>
        Future.successful(UserXiEori.NotRegistered)

      case response =>
        Future.failed(
          new XiEoriConnector.Exception(s"Request to POST $url failed because of ${response.status} ${response.body}")
        )
    }

}
