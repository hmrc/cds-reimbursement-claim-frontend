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
import cats.implicits.catsSyntaxOptionId
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.ACCEPTED
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesImpl
import play.api.libs.json.JsError
import play.api.libs.json.JsPath
import play.api.libs.json.Json
import play.api.libs.json.JsonValidationError
import play.api.mvc.Call
import play.api.test.Helpers.LOCATION
import play.api.test.Helpers._
import play.api.Configuration
import play.api.Environment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AddressLookupConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.AddressLookupConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions.TimeoutConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.arbitraryUrl
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AddressLookupServiceSpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with EitherValues
    with Matchers
    with MockFactory
    with ControllerSpec {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  private val config                 = Configuration.load(Environment.simple())
  private val servicesConfig         = new ServicesConfig(config)
  private val addressLookupConnector = mock[AddressLookupConnector]
  implicit val messages: Messages    = MessagesImpl(Lang("en"), theMessagesApi)
  private val addressLookupConfig    = new AddressLookupConfig(servicesConfig)
  private val addressLookupService   = new DefaultAddressLookupService(addressLookupConnector, addressLookupConfig)

  implicit val timeoutConfiguration: TimeoutConfig =
    TimeoutConfig(
      timeoutAmount = viewConfig.timeout,
      timeoutUrl = viewConfig.weSignedYouOutPageUrl,
      timeoutKeepAliveUrl = Some(viewConfig.ggKeepAliveUrl)
    )

  val addressUpdateCall: Call = Call("", "/update-contact-address")

  val addressLookupRequest: AddressLookupRequest = AddressLookupRequest
    .redirectBackTo(s"${viewConfig.selfBaseUrl}${addressUpdateCall.url}")
    .signOutUserVia(viewConfig.ggSignOut)
    .nameConsumerServiceAs("cds-reimbursement-claim")
    .showMax(addressLookupConfig.addressesShowLimit)
    .makeAccessibilityFooterAvailableVia(viewConfig.accessibilityStatementUrl)
    .makePhaseFeedbackAvailableVia(viewConfig.contactHmrcUrl)
    .whetherSearchOnlyUkAddresses(true)
    .whetherShowConfirmChangeText(true)
    .whetherShowSearchAgainLink(true)
    .whetherShowChangeLink(true)
    .whetherShowBanner(true)
    .disableTranslations(true)
    .withPageTitles(
      Some(s"${messages("service.title")}"),
      None,
      Some(s"${messages("address-lookup.lookup.title")} - ${messages("service.title")} - GOV.UK"),
      Some(s"${messages("address-lookup.confirm.title")} - ${messages("service.title")} - GOV.UK"),
      Some(s"${messages("address-lookup.select.title")} - ${messages("service.title")} - GOV.UK"),
      Some(s"${messages("address-lookup.edit.title")} - ${messages("service.title")} - GOV.UK"),
      Some(s"${messages("address-lookup.lookup.title")}"),
      Some(s"${messages("address-lookup.confirm.title")}"),
      Some(s"${messages("address-lookup.select.title")}"),
      Some(s"${messages("address-lookup.edit.title")}")
    )

  def mockInitiateAddressLookupResponse(request: AddressLookupRequest)(
    response: Either[Error, HttpResponse]
  ): CallHandler2[AddressLookupRequest, HeaderCarrier, EitherT[Future, Error, HttpResponse]] =
    (addressLookupConnector
      .initiate(_: AddressLookupRequest)(_: HeaderCarrier))
      .expects(request, *)
      .returning(EitherT.fromEither[Future](response))

  def mockGetAddress(id: UUID)(
    response: Either[Error, HttpResponse]
  ): CallHandler2[UUID, HeaderCarrier, EitherT[Future, Error, HttpResponse]] =
    (addressLookupConnector
      .retrieveAddress(_: UUID)(_: HeaderCarrier))
      .expects(id, *)
      .returning(EitherT.fromEither[Future](response))

  "The address lookup service" when {

    "triggering address lookup" should {

      "succeed receiving user redirect URL" in {
        val locationUrl = sample[URL]

        mockInitiateAddressLookupResponse(addressLookupRequest)(
          Right(HttpResponse(ACCEPTED, Json.obj(), headers = Map(LOCATION -> Seq(locationUrl.toString))))
        )

        val response = await(addressLookupService.startLookupRedirectingBackTo(addressUpdateCall).value)
        response.isLeft should be(false)
      }

      "fail having no request accepted" in {
        mockInitiateAddressLookupResponse(addressLookupRequest)(
          Right(HttpResponse(INTERNAL_SERVER_ERROR, Json.obj().toString()))
        )

        await(addressLookupService.startLookupRedirectingBackTo(addressUpdateCall).value).left.value should be(
          Error("The request was refused by ALF")
        )
      }

      "fail having no location header provided" in {
        mockInitiateAddressLookupResponse(addressLookupRequest)(
          Right(HttpResponse(ACCEPTED, Json.obj().toString()))
        )

        await(addressLookupService.startLookupRedirectingBackTo(addressUpdateCall).value).left.value should be(
          Error("The ALF user redirect URL is missing in the header")
        )
      }
    }

    "retrieving address" should {

      "succeed having valid address ID" in forAll { (id: UUID, address: ContactAddress) =>
        val addressLines = Seq(
          address.line1.some.toList,
          address.line2.toList,
          address.line3.toList,
          address.line4.some.toList
        ).flatten

        val json = Json.obj(
          "id"      -> id,
          "address" -> Json.obj(
            "lines"    -> addressLines,
            "postcode" -> address.postcode,
            "country"  -> Json.obj(
              "code" -> address.country.code
            )
          )
        )

        mockGetAddress(id)(Right(HttpResponse(OK, json.toString())))

        await(addressLookupService.retrieveUserAddress(id).value).value should be(address)
      }

      "fail having invalid address ID" in forAll { id: UUID =>
        mockGetAddress(id)(Right(HttpResponse(NOT_FOUND, Json.obj().toString())))

        await(addressLookupService.retrieveUserAddress(id).value).isLeft should be(true)
      }
    }

    "an address with only one address line" should {
      import DefaultAddressLookupService.addressLookupResponseReads

      "fail to deserialise" in {
        val addressJson = Json.parse("""{
                                       |    "auditRef": "101ca9ed-8dab-4868-80e3-024642e33df7",
                                       |    "address":
                                       |    {
                                       |        "lines":
                                       |        [
                                       |            "Buckingham Palace"
                                       |        ],
                                       |        "country":
                                       |        {
                                       |            "code": "GB",
                                       |            "name": "United Kingdom"
                                       |        },
                                       |        "postcode": "SW1A 1AA"
                                       |    }
                                       |}""".stripMargin)

        val path = JsPath \ "address" \ "lines"
        val err  = JsonValidationError("error.minLength", 2)
        addressJson.validate[ContactAddress] shouldBe JsError(List((path, List(err))))
      }
    }
  }
}
