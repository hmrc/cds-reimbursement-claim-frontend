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
import cats.implicits._
import cats.data.EitherT
import com.google.inject.Inject
import javax.inject.Singleton
import play.api.libs.json.Writes
import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ConnectorFailure
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ServiceUnavailableError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.TechnicalServiceError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsBusinessAssessRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsPersonalAssessRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.BusinessCompleteResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.PersonalCompleteResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps.HttpResponseOps

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

@Singleton
class BankAccountReputationConnector @Inject() (
  http: HttpClient,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  val actorSystem: ActorSystem
)(implicit
  ec: ExecutionContext
) extends Retries
    with Logging {

  def getBusinessReputation(
    data: BarsBusinessAssessRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, ConnectorError, BankAccountReputation] = {
    val url = getUri("bank-account-reputation", "business")
    for {
      httpResponse     <- getReputation(data, url)
      businessResponse <-
        EitherT.fromEither[Future](
          httpResponse
            .parseJSON[BusinessCompleteResponse]()
            .map(_.toCommonResponse())
            .leftMap(ConnectorFailure(_): ConnectorError)
        )
    } yield businessResponse
  }

  def getPersonalReputation(
    data: BarsPersonalAssessRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, ConnectorError, BankAccountReputation] = {
    val url = getUri("bank-account-reputation", "personal")
    for {
      httpResponse     <- getReputation(data, url)
      personalResponse <-
        EitherT.fromEither[Future](
          httpResponse
            .parseJSON[PersonalCompleteResponse]()
            .map(_.toCommonResponse())
            .leftMap(ConnectorFailure(_): ConnectorError)
        )
    } yield personalResponse
  }

  private def getUri(serviceName: String, apiName: String): String =
    servicesConfig.baseUrl(serviceName) + servicesConfig.getString(s"microservice.services.$serviceName.$apiName")

  lazy val retryIntervals: Seq[FiniteDuration] =
    Retries.getConfIntervals("bank-account-reputation", configuration).toList

  private def errorMessage(url: String, response: HttpResponse): String =
    s"Request to POST $url failed because of $response ${response.body}"

  private def getReputation[T](data: T, url: String)(implicit hc: HeaderCarrier, wts: Writes[T]) =
    EitherT {
      retry(retryIntervals: _*)(shouldRetry, retryReason)(
        http
          .POST[T, HttpResponse](url, data)
      )
        .map(checkResponse(_, url))
    }

  private def checkResponse(response: HttpResponse, url: String): Either[ConnectorError, HttpResponse] =
    response.status match {
      case 200                         => Right(response)
      case x if (x >= 400) & (x < 500) => Left(TechnicalServiceError(errorMessage(url, response)))
      case x if (x >= 500) & (x < 600) => Left(ServiceUnavailableError(errorMessage(url, response)))
      case _                           => Left(ConnectorFailure(errorMessage(url, response)))
    }
}
