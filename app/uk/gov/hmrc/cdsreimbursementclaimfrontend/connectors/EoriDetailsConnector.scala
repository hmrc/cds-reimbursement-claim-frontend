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

import cats.syntax.eq.*
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import org.apache.pekko.actor.ActorSystem
import play.api.Configuration
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HttpReads.Implicits.*
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Singleton
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DefaultEoriDetailsConnector])
trait EoriDetailsConnector {

  def getCurrentUserEoriDetails(implicit hc: HeaderCarrier): Future[Option[EoriDetailsConnector.Response]]
  def getEoriDetails(eori: Eori)(implicit hc: HeaderCarrier): Future[Option[EoriDetailsConnector.Response]]
}

object EoriDetailsConnector {

  final case class Response(eoriGB: Eori, eoriXI: Option[Eori], fullName: String, eoriEndDate: Option[String])
  final case class Exception(msg: String) extends scala.RuntimeException(msg)

  implicit val format: Format[Response] = Json.format[Response]
}

@Singleton
class DefaultEoriDetailsConnector @Inject() (
  http: HttpClient,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  val actorSystem: ActorSystem
)(implicit
  ec: ExecutionContext
) extends EoriDetailsConnector
    with Retries
    with Logging {

  lazy val baseUrl: String = servicesConfig.baseUrl("cds-reimbursement-claim")

  lazy val contextPath: String =
    servicesConfig.getConfString("cds-reimbursement-claim.context-path", "cds-reimbursement-claim")

  lazy val url = s"$baseUrl$contextPath/eori"

  lazy val retryIntervals: Seq[FiniteDuration] =
    Retries.getConfIntervals("cds-reimbursement-claim", configuration)

  override def getCurrentUserEoriDetails(implicit hc: HeaderCarrier): Future[Option[EoriDetailsConnector.Response]] =
    retry(retryIntervals: _*)(shouldRetry, retryReason)(
      http.GET[HttpResponse](java.net.URL(url))
    ).flatMap {
      case response if response.status === 200 =>
        Future(response.json.as[EoriDetailsConnector.Response])
          .transform(r => Some(r), e => new EoriDetailsConnector.Exception(e.getMessage))

      case response if response.status === 204 =>
        Future.successful(None)

      case response =>
        Future.failed(
          new EoriDetailsConnector.Exception(
            s"Request to GET $url failed because of ${response.status} ${response.body}"
          )
        )
    }

  override def getEoriDetails(eori: Eori)(implicit hc: HeaderCarrier): Future[Option[EoriDetailsConnector.Response]] =
    retry(retryIntervals: _*)(shouldRetry, retryReason)(
      http.GET[HttpResponse](java.net.URL(s"$url/${eori.value}"))
    ).flatMap {
      case response if response.status === 200 =>
        Future(response.json.as[EoriDetailsConnector.Response])
          .transform(r => Some(r), e => new EoriDetailsConnector.Exception(e.getMessage))

      case response if response.status === 204 =>
        Future.successful(None)

      case response =>
        Future.failed(
          new EoriDetailsConnector.Exception(
            s"Request to GET $url/${eori.value} failed because of ${response.status} ${response.body}"
          )
        )
    }

}
