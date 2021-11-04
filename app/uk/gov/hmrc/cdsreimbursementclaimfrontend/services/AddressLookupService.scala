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
import play.api.http.Status.{ACCEPTED, OK}
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsPath, JsonValidationError, Reads}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.AddressLookupConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.DefaultAddressLookupService.addressLookupResponseReads
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import java.net.URL
import java.util.UUID
import play.api.libs.json.Reads.minLength
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[DefaultAddressLookupService])
trait AddressLookupService {

  def initiate(
    request: AddressLookupRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, URL]

  def retrieveUserAddress(addressId: UUID)(implicit hc: HeaderCarrier): EitherT[Future, Error, ContactAddress]
}

class DefaultAddressLookupService @Inject() (connector: AddressLookupConnector)(implicit
  ec: ExecutionContext
) extends AddressLookupService {

  def initiate(
    request: AddressLookupRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, URL] = {

    def resolvingAddressLookupRedirectUrl(response: HttpResponse): Either[Error, URL] =
      response.header(LOCATION).map(new URL(_)) toRight Error("Could not resolve address lookup redirect URL")

    connector
      .initiate(request)
      .ensure(Error("Request was not accepted by the address lookup service"))(_.status === ACCEPTED)
      .subflatMap(resolvingAddressLookupRedirectUrl)
  }

  def retrieveUserAddress(addressId: UUID)(implicit hc: HeaderCarrier): EitherT[Future, Error, ContactAddress] = {
    def formatErrors(errors: Seq[(JsPath, Seq[JsonValidationError])]): Error =
      Error(
        errors
          .map(_.bimap(_.toString(), _.flatMap(_.messages).mkString(", ")))
          .map(pathAndErrors => s"${pathAndErrors._1}: ${pathAndErrors._2}")
          .mkString("Error parsing address lookup response:", "; ", "")
      )

    connector
      .retrieveAddress(addressId)
      .ensure(Error(s"Cannot retrieve an address by ID $addressId"))(_.status === OK)
      .subflatMap(
        _.json
          .validate[ContactAddress](addressLookupResponseReads)
          .asEither
          .leftMap(formatErrors)
      )
  }
}

object DefaultAddressLookupService {

  implicit val addressLookupResponseReads: Reads[ContactAddress] = (
    (JsPath \ "address" \ "lines").read[Array[String]](minLength[Array[String]](2)) and
      (JsPath \ "address" \ "postcode").read[String] and
      (JsPath \ "address" \ "country" \ "code").read[String].map(Country(_))
  ).apply((lines, postcode, country) =>
    lines match {
      case Array(line1, line2, line3, town) =>
        ContactAddress(line1, line2.some, line3.some, town, postcode, country)
      case Array(line1, line2, town)        =>
        ContactAddress(line1, line2.some, None, town, postcode, country)
      case Array(line1, town)               =>
        ContactAddress(line1, None, None, town, postcode, country)
    }
  )
}
