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
import cats.implicits.catsSyntaxOptionId
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.ACCEPTED
import play.api.libs.json.Json
import play.api.test.Helpers.{LOCATION, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.AddressLookupConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.AddressLookupGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.arbitraryUrl
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.net.URL
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AddressLookupServiceSpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with EitherValues
    with Matchers
    with MockFactory {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val addressLookupConnector: AddressLookupConnector = mock[AddressLookupConnector]

  val addressLookupService = new DefaultAddressLookupService(addressLookupConnector)

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
        val request     = sample[AddressLookupRequest]

        mockInitiateAddressLookupResponse(request)(
          Right(HttpResponse(ACCEPTED, Json.obj(), headers = Map(LOCATION -> Seq(locationUrl.toString))))
        )

        val response = await(addressLookupService.initiate(request).value)
        response.isLeft should be(false)
      }

      "fail having no request accepted" in {
        val request = sample[AddressLookupRequest]

        mockInitiateAddressLookupResponse(request)(
          Right(HttpResponse(INTERNAL_SERVER_ERROR, Json.obj().toString()))
        )

        await(addressLookupService.initiate(request).value).left.value should be(
          Error("Request was not accepted by the address lookup service")
        )
      }

      "fail having no location header provided" in {
        val request = sample[AddressLookupRequest]

        mockInitiateAddressLookupResponse(request)(
          Right(HttpResponse(ACCEPTED, Json.obj().toString()))
        )

        await(addressLookupService.initiate(request).value).left.value should be(
          Error("Could not resolve address lookup redirect URL")
        )
      }
    }

    "retrieving address" should {

      "succeed having valid address ID" in forAll { (id: UUID, address: NonUkAddress) =>
        val json = Json.obj(
          "id"      -> id,
          "address" -> Json.obj(
            "lines"    -> Seq(
              address.line1.some.toList,
              address.line2.toList,
              address.line3.toList,
              address.line4.some.toList
            ).flatten.seq,
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
  }
}
