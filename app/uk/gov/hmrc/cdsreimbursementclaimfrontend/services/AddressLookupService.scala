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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import cats.data.EitherT
import cats.implicits.{catsStdInstancesForFuture, catsSyntaxEq, catsSyntaxOptionId, toBifunctorOps}
import com.google.inject.{ImplementedBy, Inject}
import play.api.http.HeaderNames.LOCATION
import play.api.http.Status.ACCEPTED
import play.api.i18n.Lang
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsPath, JsonValidationError, Reads}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.AddressLookupConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.InitiateAddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.DefaultAddressLookupService.addressLookupResponseReads
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.net.URL
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[DefaultAddressLookupService])
trait AddressLookupService {

  def initiate(
    request: InitiateAddressLookupRequest
  )(implicit lang: Lang, hc: HeaderCarrier): EitherT[Future, Error, URL]

  def retrieveUserAddress(addressLocationUrl: URL)(implicit hc: HeaderCarrier): EitherT[Future, Error, NonUkAddress]
}

class DefaultAddressLookupService @Inject() (connector: AddressLookupConnector)(implicit
  ec: ExecutionContext
) extends AddressLookupService {

  def initiate(
    request: InitiateAddressLookupRequest
  )(implicit lang: Lang, hc: HeaderCarrier): EitherT[Future, Error, URL] = {

    def resolvingAddressLookupRedirectUrl(response: HttpResponse): Either[Error, URL] =
      response.header(LOCATION).map(new URL(_)) toRight Error("Could not resolve address lookup redirect URL")

    connector
      .initiate(request)
      .ensure(Error("Request was not accepted by the address lookup service"))(_.status === ACCEPTED)
      .subflatMap(resolvingAddressLookupRedirectUrl)
  }

  def retrieveUserAddress(addressLocationUrl: URL)(implicit hc: HeaderCarrier): EitherT[Future, Error, NonUkAddress] = {
    def formatErrors(errors: Seq[(JsPath, Seq[JsonValidationError])]): Error =
      Error(
        errors
          .map(_.bimap(_.toString(), _.flatMap(_.messages).mkString(", ")))
          .map(pathAndErrors => s"${pathAndErrors._1}: ${pathAndErrors._2}")
          .mkString("Error parsing address lookup response:", "; ", "")
      )

    connector
      .retrieveAddress(addressLocationUrl)
      .subflatMap(
        _.json
          .validate[NonUkAddress](addressLookupResponseReads)
          .asEither
          .leftMap(formatErrors)
      )
  }
}

object DefaultAddressLookupService {

  implicit val addressLookupResponseReads: Reads[NonUkAddress] = (
    (JsPath \ "address" \ "lines").read[Array[String]] and
      (JsPath \ "address" \ "postcode").read[String] and
      (JsPath \ "address" \ "country" \ "code").read[String].map(Country(_))
  ).apply((lines, postcode, country) =>
    lines match {
      case Array(line1, line2, line3, town) =>
        NonUkAddress(line1, line2.some, line3.some, town, postcode, country)
      case Array(line1, line2, town) =>
        NonUkAddress(line1, line2.some, None, town, postcode, country)
      case Array(line1, town) =>
        NonUkAddress(line1, None, None, town, postcode, country)
    }
  )
}
