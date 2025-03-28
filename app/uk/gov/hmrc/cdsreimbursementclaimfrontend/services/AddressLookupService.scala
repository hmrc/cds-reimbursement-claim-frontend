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
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.Reads.minLength
import play.api.libs.json.JsPath
import play.api.libs.json.Json
import play.api.libs.json.JsonValidationError
import play.api.libs.json.Reads
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AddressLookupConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.AddressLookupConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions.TimeoutConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupPageLabels
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.LabelsByLocale
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.DefaultAddressLookupService.addressLookupResponseReads
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier

import java.net.URI
import java.net.URL
import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DefaultAddressLookupService])
trait AddressLookupService {

  def startLookupRedirectingBackTo(
    addressUpdateUrl: Call
  )(implicit hc: HeaderCarrier, messages: Messages): EitherT[Future, Error, URL]

  def retrieveUserAddress(addressId: UUID)(implicit hc: HeaderCarrier): EitherT[Future, Error, ContactAddress]
}

class DefaultAddressLookupService @Inject() (
  connector: AddressLookupConnector,
  addressLookupConfiguration: AddressLookupConfig,
  messagesApi: MessagesApi
)(implicit
  ec: ExecutionContext,
  viewConfig: ViewConfig
) extends AddressLookupService
    with Logging {

  implicit val timeoutConfiguration: TimeoutConfig =
    TimeoutConfig(
      timeoutAmount = viewConfig.ggTimeoutSeconds,
      timeoutUrl = viewConfig.weSignedYouOutPageUrl,
      timeoutKeepAliveUrl = Some(viewConfig.ggKeepAliveUrl)
    )

  val englishMessages: Messages = MessagesImpl(Lang("en"), messagesApi)
  val welshMessages: Messages   = MessagesImpl(Lang("cy"), messagesApi)

  private def appTitle(messages: Messages): String =
    messages("service.title")

  private def fullPageTitle(titleKey: String, messages: Messages): String =
    viewConfig
      .pageTitleWithServiceName(
        messages(titleKey),
        messages("service.title")
      )

  private def pageHeading(titleKey: String, messages: Messages): String =
    messages(titleKey)

  def startLookupRedirectingBackTo(
    addressUpdateUrl: Call
  )(implicit hc: HeaderCarrier, messages: Messages): EitherT[Future, Error, URL] = {
    val request: AddressLookupRequest =
      AddressLookupRequest
        .redirectBackTo(s"${viewConfig.selfBaseUrl}${addressUpdateUrl.url}")
        .signOutUserVia(viewConfig.ggSignOut)
        .nameConsumerServiceAs("cds-reimbursement-claim")
        .withPageLabels(
          LabelsByLocale(
            en = AddressLookupPageLabels(
              appTitle = appTitle(englishMessages).some,
              phaseBannerHtml = None,
              lookupTitle = fullPageTitle("address-lookup.lookup.title", englishMessages).some,
              confirmTitle = fullPageTitle("address-lookup.confirm.title", englishMessages).some,
              selectTitle = fullPageTitle("address-lookup.select.title", englishMessages).some,
              editTitle = fullPageTitle("address-lookup.edit.title", englishMessages).some,
              lookupHeading = pageHeading("address-lookup.lookup.title", englishMessages).some,
              confirmHeading = pageHeading("address-lookup.confirm.h1", englishMessages).some,
              selectHeading = pageHeading("address-lookup.select.title", englishMessages).some,
              editHeading = pageHeading("address-lookup.edit.title", englishMessages).some,
              searchAgainLinkText = Some(englishMessages("address-lookup.label.searchAgainLinkText"))
            ),
            cy = AddressLookupPageLabels(
              appTitle = appTitle(welshMessages).some,
              phaseBannerHtml = None,
              lookupTitle = fullPageTitle("address-lookup.lookup.title", welshMessages).some,
              confirmTitle = fullPageTitle("address-lookup.confirm.title", welshMessages).some,
              selectTitle = fullPageTitle("address-lookup.select.title", welshMessages).some,
              editTitle = fullPageTitle("address-lookup.edit.title", welshMessages).some,
              lookupHeading = pageHeading("address-lookup.lookup.title", welshMessages).some,
              confirmHeading = pageHeading("address-lookup.confirm.h1", welshMessages).some,
              selectHeading = pageHeading("address-lookup.select.title", welshMessages).some,
              editHeading = pageHeading("address-lookup.edit.title", welshMessages).some,
              searchAgainLinkText = Some(welshMessages("address-lookup.label.searchAgainLinkText"))
            )
          )
        )
        .showMax(addressLookupConfiguration.addressesShowLimit)
        .makeAccessibilityFooterAvailableVia(viewConfig.accessibilityStatementUrl)
        .makePhaseFeedbackAvailableVia(viewConfig.contactHmrcUrl)
        .whetherSearchOnlyUkAddresses(true)
        .whetherShowConfirmChangeText(true)
        .whetherShowSearchAgainLink(true)
        .whetherShowChangeLink(true)
        .whetherShowBanner(true)
        .disableTranslations(!viewConfig.enableLanguageSwitching)

    logger.debug(s"Making ALF call sending payload:\n${Json.prettyPrint(Json.toJson(request))}")

    connector
      .initiate(request)
      .subflatMap { response =>
        logger.debug(s"Received ALF response with status ${response.status} and body '${response.body}'")
        if response.status === ACCEPTED then
          response
            .header(LOCATION)
            .map(URI.create(_).toURL())
            .toRight(Error("The ALF user redirect URL is missing in the header"))
        else Left(Error("The request was refused by ALF"))
      }
  }

  def retrieveUserAddress(addressId: UUID)(implicit hc: HeaderCarrier): EitherT[Future, Error, ContactAddress] = {
    def formatErrors(errors: scala.collection.Seq[(JsPath, scala.collection.Seq[JsonValidationError])]): Error =
      Error(
        errors
          .map(_.bimap(_.toString(), _.flatMap(_.messages).mkString(", ")))
          .map(pathAndErrors => s"${pathAndErrors._1}: ${pathAndErrors._2}")
          .mkString("Error parsing address lookup response:", "; ", "")
      )

    logger.debug(s"Retrieving ALF user address by ID: ${addressId.toString}")

    connector
      .retrieveAddress(addressId)
      .ensure(Error(s"Cannot retrieve an address by ID $addressId"))(_.status === OK)
      .subflatMap { response =>
        logger.debug(s"Received ALF response with status ${response.status} and body '${response.body}'")
        if response.status === OK then
          response.json
            .validate[ContactAddress](addressLookupResponseReads)
            .asEither
            .leftMap(formatErrors)
        else Left(Error("Failed to retrieve ALF address"))
      }
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
      case _                                =>
        ContactAddress("", None, None, "", postcode, country)
    }
  )
}
