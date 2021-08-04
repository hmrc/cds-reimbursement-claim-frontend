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
import play.api.http.HeaderNames.ACCEPT_LANGUAGE
import play.api.i18n.Lang
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AddressLookupConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.InitiateAddressLookupRequest
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}

import java.net.URL
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@ImplementedBy(classOf[DefaultAddressLookupConnector])
trait AddressLookupConnector {

  def initiate(
    request: InitiateAddressLookupRequest
  )(implicit lang: Lang, hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def retrieveAddress(addressUrl: URL)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]
}

class DefaultAddressLookupConnector @Inject() (
  http: HttpClient,
  serviceConfig: AddressLookupConfig
)(implicit ec: ExecutionContext)
    extends AddressLookupConnector {

  def initiate(
    request: InitiateAddressLookupRequest
  )(implicit lang: Lang, hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = EitherT {
    http
      .POST[InitiateAddressLookupRequest, HttpResponse](
        serviceConfig.triggerAddressLookupUrl,
        request,
        Seq(ACCEPT_LANGUAGE -> lang.language)
      )
      .map(Right(_))
      .recover { case NonFatal(e) =>
        Left(Error(e))
      }
  }

  def retrieveAddress(addressLocationUrl: URL)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT {
      http
        .GET[HttpResponse](s"${serviceConfig.retrieveAddressUrl}?id=$addressLocationUrl")
        .map(Right(_))
        .recover { case NonFatal(e) =>
          Left(Error(e))
        }
    }
}
