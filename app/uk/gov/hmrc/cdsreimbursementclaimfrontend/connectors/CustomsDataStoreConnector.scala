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

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject}
import javax.inject.Singleton
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Eori, Error}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[DefaultCustomsDataStoreConnector])
trait CustomsDataStoreConnector {
  def getEmailByEori(eori: Eori)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]
}

@Singleton
class DefaultCustomsDataStoreConnector @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) extends CustomsDataStoreConnector
    with Logging {

  val serviceName: String = "customs-data-store"
  val emailByEori: String = "email-by-eori"

  def getUri(eori: Eori): String = {
    val url =
      servicesConfig.baseUrl(serviceName) + servicesConfig.getString(s"microservice.services.$serviceName.$emailByEori")
    url.replaceAll("/:eori/", s"/${eori.value}/")
  }

  def getEmailByEori(eori: Eori)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .GET[HttpResponse](getUri(eori))
        .map(Right(_))
        .recover { case e =>
          logger.warn("Error calling Customs Data Store", e)
          Left(Error(e))
        }
    )

}
