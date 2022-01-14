/*
 * Copyright 2022 HM Revenue & Customs
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
import cats.implicits.catsStdInstancesForFuture
import cats.implicits.catsSyntaxEq
import cats.implicits.catsSyntaxOptionId
import cats.implicits.toBifunctorOps
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import play.api.http.HeaderNames.LOCATION
import play.api.http.Status.ACCEPTED
import play.api.http.Status.OK
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.JsPath
import play.api.libs.json.JsonValidationError
import play.api.libs.json.Reads
import play.api.libs.json.Reads.minLength
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AddressLookupConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.AddressLookupConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions.TimeoutConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.DefaultAddressLookupService.addressLookupResponseReads
import uk.gov.hmrc.http.HeaderCarrier

import java.net.URL
import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DefaultAddressLookupService])
trait AddressLookupService {

  def startLookupRedirectingBackTo(addressUpdateUrl: Call)(implicit hc: HeaderCarrier): EitherT[Future, Error, URL]

  def retrieveUserAddress(addressId: UUID)(implicit hc: HeaderCarrier): EitherT[Future, Error, ContactAddress]
}

class DefaultAddressLookupService @Inject() (
  connector: AddressLookupConnector,
  addressLookupConfiguration: AddressLookupConfig
)(implicit
  ec: ExecutionContext,
  viewConfig: ViewConfig
) extends AddressLookupService {

  implicit val addressLookupTimeoutConfiguration: TimeoutConfig =
    TimeoutConfig(
      timeoutAmount = viewConfig.timeout,
      timeoutUrl = baseRoutes.StartController.timedOut().url,
      timeoutKeepAliveUrl = Some(viewConfig.buildCompleteSelfUrl(viewConfig.ggKeepAliveUrl))
    )

  def startLookupRedirectingBackTo(addressUpdateUrl: Call)(implicit hc: HeaderCarrier): EitherT[Future, Error, URL] = {
    val request =
      AddressLookupRequest
        .redirectBackTo(addressUpdateUrl)
        .signOutUserVia(viewConfig.signOutUrl)
        .nameConsumerServiceAs("cds-reimbursement-claim")
        .showMax(addressLookupConfiguration.addressesShowLimit)
        .makeAccessibilityFooterAvailableVia(viewConfig.accessibilityStatementUrl)
        .makePhaseFeedbackAvailableVia(viewConfig.contactHmrcUrl)
        .whetherSearchOnlyUkAddresses(true)
        .whetherShowConfirmChangeText(true)
        .whetherShowSearchAgainLink(true)
        .whetherShowChangeLink(true)
        .whetherShowBanner(true)

    connector
      .initiate(request)
      .ensure(Error("Request was not accepted by the address lookup service"))(_.status === ACCEPTED)
      .subflatMap(response =>
        response.header(LOCATION).map(new URL(_)) toRight Error("Could not resolve address lookup redirect URL")
      )
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

  def isInvalidAddressError(error: Error): Boolean =
    error.message.contains("/address/postcode: error.path.missing") ||
      error.message.contains("/address/lines: error.minLength")
}
