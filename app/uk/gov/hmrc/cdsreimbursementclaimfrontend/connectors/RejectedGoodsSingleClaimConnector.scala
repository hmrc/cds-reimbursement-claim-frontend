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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import akka.actor.ActorSystem
import cats.syntax.eq._
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.Configuration

@ImplementedBy(classOf[RejectedGoodsSingleClaimConnectorImpl])
trait RejectedGoodsSingleClaimConnector {

  def submitClaim(claimRequest: RejectedGoodsSingleClaimConnector.Request)(implicit
    hc: HeaderCarrier
  ): Future[RejectedGoodsSingleClaimConnector.Response]
}

object RejectedGoodsSingleClaimConnector {

  final case class Request(claim: RejectedGoodsSingleJourney.Output)
  final case class Response(caseNumber: String)
  final case class Exception(msg: String) extends scala.RuntimeException(msg)

  implicit val requestFormat: Format[Request]   = Json.format[Request]
  implicit val responseFormat: Format[Response] = Json.format[Response]
}

@Singleton
class RejectedGoodsSingleClaimConnectorImpl @Inject() (
  http: HttpClient,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  val actorSystem: ActorSystem
)(implicit
  ec: ExecutionContext
) extends RejectedGoodsSingleClaimConnector
    with Retries {

  import RejectedGoodsSingleClaimConnector._

  val baseUrl: String                     = servicesConfig.baseUrl("cds-reimbursement-claim")
  val claimUrl: String                    = s"$baseUrl/claims/rejected-goods-single"
  val retryIntervals: Seq[FiniteDuration] = getConfIntervals("cds-reimbursement-claim", configuration)

  override def submitClaim(claimRequest: Request)(implicit
    hc: HeaderCarrier
  ): Future[Response] =
    retry(retryIntervals: _*)(shouldRetry, retryReason)(
      http
        .POST[Request, HttpResponse](
          claimUrl,
          claimRequest
        )
    ).flatMap(response =>
      if (response.status === 200)
        response
          .parseJSON[Response]()
          .fold(error => Future.failed(new Exception(error)), Future.successful(_))
      else
        Future.failed(
          new Exception(s"Request to POST $claimUrl failed because of $response ${response.body}")
        )
    )
}
