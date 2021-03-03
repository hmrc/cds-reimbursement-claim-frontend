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
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.{CDSReimbursementClaimConnector, ClaimConnector}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.{BarsBusinessAssessRequest, BarsPersonalAssessRequest}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.{BusinessCompleteResponse, PersonalCompleteResponse, ReputationErrorResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountReputationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ClaimServiceSpec extends AnyWordSpec with Matchers with MockFactory {

  implicit val hc: HeaderCarrier   = HeaderCarrier()
  implicit val request: Request[_] = FakeRequest()

  //private val defaultLanguage    = Lang.defaultLang
  private val claimConnector              = mock[ClaimConnector]
  private val reimbursementClaimConnector = mock[CDSReimbursementClaimConnector]
  private val submitClaimService          = new DefaultClaimService(claimConnector, reimbursementClaimConnector)

  val okResponse: JsValue = Json.parse("""{
                                         |    "PostNewClaimsResponse": {
                                         |        "ResponseCommon": {
                                         |            "Status": "OK",
                                         |            "ProcessingDateTime": "2020-12-23T16:58:28Z",
                                         |			"CDFPayCaseNumber": "NDRC-1234",
                                         |			"CDFPayService":"NDRC"
                                         |        }
                                         |    }
                                         |}""".stripMargin)

  def errorResponse(errorMessage: String): JsValue = Json.parse(s"""{
                                                                   |   "ErrorDetails":{
                                                                   |      "ProcessingDateTime":"2016-10-10T13:52:16Z",
                                                                   |      "CorrelationId":"d60de98c-f499-47f5-b2d6-e80966e8d19e",
                                                                   |      "ErrorMessage":"$errorMessage"
                                                                   |    }
                                                                   |}""".stripMargin)

//  def mockSubmitClaim(submitClaimData: JsValue)(response: Either[Error, HttpResponse]) =
//    (claimConnector
//      .submitClaim(_: JsValue, _: Lang)(_: HeaderCarrier))
//      .expects(submitClaimData, *, *)
//      .returning(EitherT.fromEither[Future](response))
//      .atLeastOnce()

  //  "Submit Claim Service" when {
  //    "handling submit claim" should {
  //      "handle successful submits" when {
  //        "there is a valid payload" in {
  //          mockSubmitClaim(JsString("Hello"))(Right(HttpResponse(200, okResponse, Map.empty[String, Seq[String]])))
  //          await(submitClaimService.submitClaim(JsString("Hello"), defaultLanguage).value) shouldBe Right(okResponse)
  //        }
  //      }
  //
  //      "handle unsuccessful submits" when {
  //        "500 response" in {
  //          val eisResponse = errorResponse("call to get submit claim came back with status")
  //          mockSubmitClaim(JsString("Hello"))(Right(HttpResponse(500, eisResponse, Map.empty[String, Seq[String]])))
  //          val response    = await(submitClaimService.submitClaim(JsString("Hello"), defaultLanguage).value)
  //          response.fold(_.message should include("call to get submit claim came back with status"), _ => fail())
  //        }
  //
  //        "Invalid Json response" in {
  //          mockSubmitClaim(JsString("Hello"))(Right(HttpResponse(200, """{"a"-"b"}""", Map.empty[String, Seq[String]])))
  //          val response = await(submitClaimService.submitClaim(JsString("Hello"), defaultLanguage).value)
  //          response.fold(_.message should include("Unexpected character"), _ => fail())
  //        }
  //
  //      }
  //    }
  //  }

  "Business Reputation Service" should {

    def mockBusinessReputationConnector(data: JsValue)(response: Either[Error, HttpResponse]) =
      (reimbursementClaimConnector
        .getBusinessReputation(_: JsValue)(_: HeaderCarrier))
        .expects(data, *)
        .returning(EitherT.fromEither[Future](response))
        .atLeastOnce()

    "retrieve and parse data" in {
      val businessRequest = sample[BarsBusinessAssessRequest]
      val businessReponse = sample[BusinessCompleteResponse]
      val httpResponse    = HttpResponse(200, Json.toJson(businessReponse).toString())
      mockBusinessReputationConnector(Json.toJson(businessRequest))(Right(httpResponse))
      val response        = await(submitClaimService.getBusinessAccountReputation(businessRequest).value)
      response shouldBe Right(businessReponse.toCommonResponse())
    }

    "parse error response" in {
      val businessRequest = sample[BarsBusinessAssessRequest]
      val errorResponse   = sample[ReputationErrorResponse]
      val httpResponse    = HttpResponse(400, Json.toJson(errorResponse).toString())
      mockBusinessReputationConnector(Json.toJson(businessRequest))(Right(httpResponse))
      val response        = await(submitClaimService.getBusinessAccountReputation(businessRequest).value)
      response shouldBe Right(errorResponse.toCommonResponse())
    }

    "Fail when the connector fails" in {
      val businessRequest = sample[BarsBusinessAssessRequest]
      val httpResponse    = HttpResponse(500, "")
      mockBusinessReputationConnector(Json.toJson(businessRequest))(Right(httpResponse))
      val response        = await(submitClaimService.getBusinessAccountReputation(businessRequest).value)
      response shouldBe Left(Error("Call to Business Reputation Service (BARS) failed with: 500, body: "))
    }

    "Fail when the returned JSON cannot be parsed" in {
      val businessRequest = sample[BarsBusinessAssessRequest]
      val httpResponse    = HttpResponse(200, """{"BARS" : "maybe not"}""")
      mockBusinessReputationConnector(Json.toJson(businessRequest))(Right(httpResponse))
      val response        = await(submitClaimService.getBusinessAccountReputation(businessRequest).value)
      response.isLeft                                                                      shouldBe true
      response.left.getOrElse(fail).message.contains("could not parse http response JSON") shouldBe true
    }

  }

  "Personal Reputation Service" should {

    val spr             = sample[BarsPersonalAssessRequest]
    val personalRequest =
      spr.copy(subject = spr.subject.copy(name = None, firstName = Some("Joe"), lastName = Some("White")))

    def mockPersonalReputationConnector(data: JsValue)(response: Either[Error, HttpResponse]) =
      (reimbursementClaimConnector
        .getPersonalReputation(_: JsValue)(_: HeaderCarrier))
        .expects(data, *)
        .returning(EitherT.fromEither[Future](response))
        .atLeastOnce()

    "retrieve and parse data" in {
      val personalReponse = sample[PersonalCompleteResponse]
      val httpResponse    = HttpResponse(200, Json.toJson(personalReponse).toString())
      mockPersonalReputationConnector(Json.toJson(personalRequest))(Right(httpResponse))
      val response        = await(submitClaimService.getPersonalAccountReputation(personalRequest).value)
      response shouldBe Right(personalReponse.toCommonResponse())
    }

    "parse error response" in {
      val errorResponse = sample[ReputationErrorResponse]
      val httpResponse  = HttpResponse(400, Json.toJson(errorResponse).toString())
      mockPersonalReputationConnector(Json.toJson(personalRequest))(Right(httpResponse))
      val response      = await(submitClaimService.getPersonalAccountReputation(personalRequest).value)
      response shouldBe Right(errorResponse.toCommonResponse())
    }

    "Fail when the connector fails" in {
      val httpResponse = HttpResponse(500, "")
      mockPersonalReputationConnector(Json.toJson(personalRequest))(Right(httpResponse))
      val response     = await(submitClaimService.getPersonalAccountReputation(personalRequest).value)
      response shouldBe Left(Error("Call to Business Reputation Service (BARS) failed with: 500, body: "))
    }

    "Fail when the returned JSON cannot be parsed" in {
      val httpResponse = HttpResponse(200, """{"BARS" : "maybe not"}""")
      mockPersonalReputationConnector(Json.toJson(personalRequest))(Right(httpResponse))
      val response     = await(submitClaimService.getPersonalAccountReputation(personalRequest).value)
      response.isLeft                                                                      shouldBe true
      response.left.getOrElse(fail).message.contains("could not parse http response JSON") shouldBe true
    }

  }

}
