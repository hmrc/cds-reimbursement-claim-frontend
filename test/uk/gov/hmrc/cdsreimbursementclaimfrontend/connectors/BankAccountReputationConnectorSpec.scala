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

import com.typesafe.config.ConfigFactory
import org.apache.pekko.actor.ActorSystem
import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.test.Helpers.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ConnectorFailure
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.ServiceUnavailableError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ConnectorError.TechnicalServiceError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.BankAccountReputation
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsBusinessAssessRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.BarsPersonalAssessRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.BusinessCompleteResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.PersonalCompleteResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountReputationGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountReputationGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.http.HttpReads
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.*

class BankAccountReputationConnectorSpec
    extends AnyWordSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with HttpV2Support
    with ConnectorSpec
    with BeforeAndAfterAll
    with TypeCheckedTripleEquals {

  val config = Configuration(ConfigFactory.load)

  val actorSystem = ActorSystem("test-BankAccountReputationConnector")

  override protected def afterAll(): Unit =
    actorSystem.terminate()

  val connector = new BankAccountReputationConnector(mockHttp, new ServicesConfig(config), config, actorSystem)

  val businessRequest: BarsBusinessAssessRequest     = sample[BarsBusinessAssessRequest]
  val personalRequest: BarsPersonalAssessRequest     = sample[BarsPersonalAssessRequest]
  val businessResponseBody: BusinessCompleteResponse = sample(
    BankAccountReputationGen.arbitraryBusinessCompleteResponse.arbitrary
  )
  val personalResponseBody: PersonalCompleteResponse = sample(
    BankAccountReputationGen.arbitraryPersonalCompleteResponse.arbitrary
  )
  val businessUrl                                    = "http://localhost:7502/verify/business"
  val personalUrl                                    = "http://localhost:7502/verify/personal"

  def givenServiceReturns(expectedUrl: String, request: JsValue)(
    response: HttpResponse
  ): CallHandler2[HttpReads[HttpResponse], ExecutionContext, Future[HttpResponse]] =
    mockHttpPostSuccess(expectedUrl, request)(response)

  "BankAccountReputationConnector" should {
    "have retries defined" in {
      connector.retryIntervals should ===(Seq(1.second, 2.seconds))
    }

    "return bank account reputation information from a successful business request call" in
      forAll { (request: BarsBusinessAssessRequest, response: BusinessCompleteResponse) =>
        givenServiceReturns(businessUrl, Json.toJson(request))(
          HttpResponse(200, Json.toJson(response).toString())
        ).once()
        await(connector.getBusinessReputation(request).value) shouldBe a[Right[?, BankAccountReputation]]
      }

    "return bank account reputation information from a successful personal request call" in {
      forAll { (request: BarsPersonalAssessRequest, response: PersonalCompleteResponse) =>
        givenServiceReturns(personalUrl, Json.toJson(request))(
          HttpResponse(200, Json.toJson(response).toString())
        ).once()
        await(connector.getPersonalReputation(request).value) shouldBe a[Right[?, BankAccountReputation]]
      }
    }

    "return an error when business response is 200 but payload is empty" in {
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(
        HttpResponse(200, "")
      ).once()
      await(connector.getBusinessReputation(businessRequest).value) should ===(
        Left(
          ConnectorFailure(
            "could not read http response as JSON: No content to map due to end-of-input\n at [Source: (String)\"\"; line: 1, column: 0]"
          )
        )
      )
    }

    "return an error when personal response is 200 but payload is empty" in {
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(HttpResponse(200, "")).once()
      await(connector.getPersonalReputation(personalRequest).value) should ===(
        Left(
          ConnectorFailure(
            "could not read http response as JSON: No content to map due to end-of-input\n at [Source: (String)\"\"; line: 1, column: 0]"
          )
        )
      )
    }

    "return an error when business response is 200 but payload is invalid" in {
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(HttpResponse(200, """{"foo":"bar"}""")).once()
      await(connector.getBusinessReputation(businessRequest).value) should ===(
        Left(
          ConnectorFailure(
            "could not parse http response JSON: /accountNumberIsWellFormatted: [error.path.missing]"
          )
        )
      )
    }

    "return an error when personal response is 200 but payload is invalid" in {
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(HttpResponse(200, """{"foo":"bar"}""")).once()
      await(connector.getPersonalReputation(personalRequest).value) should ===(
        Left(
          ConnectorFailure(
            "could not parse http response JSON: /accountNumberIsWellFormatted: [error.path.missing]"
          )
        )
      )
    }

    "return an error when business request returns invalid success response status" in {
      val response = Json.toJson(businessResponseBody).toString()
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(
        HttpResponse(201, Json.toJson(response).toString())
      )
        .once()
      await(connector.getBusinessReputation(businessRequest).value) should ===(
        Left(
          ConnectorFailure(
            s"""Request to POST http://localhost:7502/verify/business failed because of HttpResponse status=201 "${response
                .replace("\"", "\\\"")}""""
          )
        )
      )
    }

    "return an error when personal request returns invalid success response status" in {
      val response = Json.toJson(personalResponseBody).toString()
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(
        HttpResponse(201, Json.toJson(response).toString())
      )
        .once()
      await(connector.getPersonalReputation(personalRequest).value) should ===(
        Left(
          ConnectorFailure(
            s"""Request to POST http://localhost:7502/verify/personal failed because of HttpResponse status=201 "${response
                .replace("\"", "\\\"")}""""
          )
        )
      )
    }

    "return an error when business request returns 400 response status" in {
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(
        HttpResponse(400, """{"code": "MALFORMED_JSON", "desc": "foo: bar"}""")
      ).once()
      await(connector.getBusinessReputation(businessRequest).value) should ===(
        Left(
          TechnicalServiceError(
            """Request to POST http://localhost:7502/verify/business failed because of HttpResponse status=400 {"code": "MALFORMED_JSON", "desc": "foo: bar"}"""
          )
        )
      )
    }

    "return an error when business request returns 404 response status" in {
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(HttpResponse(404, "not found")).once()
      await(connector.getBusinessReputation(businessRequest).value) should ===(
        Left(
          TechnicalServiceError(
            "Request to POST http://localhost:7502/verify/business failed because of HttpResponse status=404 not found"
          )
        )
      )
    }

    "return an error when personal request returns 400 response status" in {
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(
        HttpResponse(400, """{"code": "MALFORMED_JSON", "desc": "foo: bar"}""")
      ).once()
      await(connector.getPersonalReputation(personalRequest).value) shouldBe (
        Left(
          TechnicalServiceError(
            """Request to POST http://localhost:7502/verify/personal failed because of HttpResponse status=400 {"code": "MALFORMED_JSON", "desc": "foo: bar"}"""
          )
        )
      )
    }

    "return an error when personal request returns 404 response status" in {
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(HttpResponse(404, "not found")).once()
      await(connector.getPersonalReputation(personalRequest).value) shouldBe (
        Left(
          TechnicalServiceError(
            "Request to POST http://localhost:7502/verify/personal failed because of HttpResponse status=404 not found"
          )
        )
      )
    }

    "return an error when 5xx response status from a business request in the third attempt" in {
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(HttpResponse(500, "")).once()
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(HttpResponse(500, "")).once()
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(HttpResponse(500, "")).once()
      await(connector.getBusinessReputation(businessRequest).value) should ===(
        Left(
          ServiceUnavailableError(
            "Request to POST http://localhost:7502/verify/business failed because of HttpResponse status=500 "
          )
        )
      )
    }

    "return an error when 5xx response status from a personal request in the third attempt" in {
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(HttpResponse(500, "")).once()
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(HttpResponse(500, "")).once()
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(HttpResponse(500, "")).once()
      await(connector.getPersonalReputation(personalRequest).value) should ===(
        Left(
          ServiceUnavailableError(
            "Request to POST http://localhost:7502/verify/personal failed because of HttpResponse status=500 "
          )
        )
      )
    }

    "accept valid response from a business request on a second attempt" in {
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(HttpResponse(500, "")).once()
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(
        HttpResponse(200, Json.toJson(businessResponseBody).toString())
      ).once()
      await(connector.getBusinessReputation(businessRequest).value) shouldBe a[Right[?, BankAccountReputation]]
    }

    "accept valid response from a personal request on a second attempt" in {
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(HttpResponse(500, "")).once()
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(
        HttpResponse(200, Json.toJson(personalResponseBody).toString())
      ).once()
      await(connector.getPersonalReputation(personalRequest).value) shouldBe a[Right[?, BankAccountReputation]]
    }

    "accept valid response from a business request on a third attempt" in {
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(HttpResponse(500, "")).once()
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(HttpResponse(500, "")).once()
      givenServiceReturns(businessUrl, Json.toJson(businessRequest))(
        HttpResponse(200, Json.toJson(businessResponseBody).toString())
      ).once()
      await(connector.getBusinessReputation(businessRequest).value) shouldBe a[Right[?, BankAccountReputation]]
    }

    "accept valid response from a personal request on a third attempt" in {
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(HttpResponse(500, "")).once()
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(HttpResponse(500, "")).once()
      givenServiceReturns(personalUrl, Json.toJson(personalRequest))(
        HttpResponse(200, Json.toJson(personalResponseBody).toString())
      ).once()
      await(connector.getPersonalReputation(personalRequest).value) shouldBe a[Right[?, BankAccountReputation]]
    }
  }
}
