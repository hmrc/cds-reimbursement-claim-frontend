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
import org.scalatest.Ignore
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.i18n.Lang
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Request
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Ignore
class ClaimServiceSpec extends AnyWordSpec with Matchers with MockFactory {

  implicit val hc: HeaderCarrier   = HeaderCarrier()
  implicit val request: Request[_] = FakeRequest()

  //private val defaultLanguage    = Lang.defaultLang
  private val claimConnector = mock[ClaimConnector]
  //private val backEndConnector   = mock[CDSReimbursementClaimConnector]
  // private val submitClaimService = new DefaultClaimService(claimConnector, backEndConnector)

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

  def mockSubmitClaim(submitClaimData: JsValue)(response: Either[Error, HttpResponse]) =
    (claimConnector
      .submitClaim(_: JsValue, _: Lang)(_: HeaderCarrier))
      .expects(submitClaimData, *, *)
      .returning(EitherT.fromEither[Future](response))
      .atLeastOnce()

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

}
