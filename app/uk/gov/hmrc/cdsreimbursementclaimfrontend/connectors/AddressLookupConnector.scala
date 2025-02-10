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

import cats.data.EitherT
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AddressLookupConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.http.HttpReads.Implicits.*
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.http.client.HttpClientV2
import play.api.libs.ws.JsonBodyWritables.*

import java.net.URL
import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal

@ImplementedBy(classOf[DefaultAddressLookupConnector])
trait AddressLookupConnector {

  def initiate(
    request: AddressLookupRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def retrieveAddress(addressId: UUID)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]
}

class DefaultAddressLookupConnector @Inject() (
  http: HttpClientV2,
  addressLookupServiceConfig: AddressLookupConfig
)(implicit ec: ExecutionContext)
    extends AddressLookupConnector {

  def initiate(
    request: AddressLookupRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = EitherT {
    http
      .post(addressLookupServiceConfig.startLookupUrl)
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
      .map(Right(_))
      .recover { case NonFatal(e) =>
        Left(Error(e))
      }
  }

  def retrieveAddress(addressId: UUID)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT {
      http
        .get(URL(s"${addressLookupServiceConfig.retrieveAddressUrl}?id=$addressId"))
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case NonFatal(e) =>
          Left(Error(e))
        }
    }
}
