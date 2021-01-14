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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import org.scalamock.matchers.ArgCapture.CaptureOne
import org.scalamock.scalatest.MockFactory
import play.api.http.Status
import play.api.libs.json._
import play.api.test.Helpers._
import play.api.test._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.test.ReimbursementSpec
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpReads, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

class PostNewClaimControllerSpec extends {
  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
} with ReimbursementSpec with MockFactory with DefaultAwaitTimeout {

  implicit val httpClient   = mock[HttpClient]
  implicit val materialiser = NoMaterializer
  private val controller    = new PostNewClaimController

  "POST" should {
    "return 200 on GET Request" in {
      val response            = JsObject(Seq("hello" -> JsString("word")))
      val capturedRequestBody = CaptureOne[JsValue]()
      (
        httpClient
          .POST[JsValue, HttpResponse](_: String, _: JsValue, _: Seq[(String, String)])(
            _: Writes[JsValue],
            _: HttpReads[HttpResponse],
            _: HeaderCarrier,
            _: ExecutionContext
          )
        )
        .expects(*, capture(capturedRequestBody), *, *, *, *, *)
        .returning(Future.successful(HttpResponse(OK, Json.stringify(response))))
        .atLeastOnce()

      val fakeRequest = FakeRequest("GET", "/")
      val result      = controller.claim()(fakeRequest)
      capturedRequestBody.value.toString().length shouldBe >(500)
      status(result)                              shouldBe Status.OK
      contentAsJson(result)                       shouldBe response
    }

    "return 200 on POST Request with valid JSON body" in {
      val response = JsObject(Seq("hello" -> JsString("word")))
      (
        httpClient
          .POST[JsValue, HttpResponse](_: String, _: JsValue, _: Seq[(String, String)])(
            _: Writes[JsValue],
            _: HttpReads[HttpResponse],
            _: HeaderCarrier,
            _: ExecutionContext
          )
        )
        .expects(*, *, *, *, *, *, *)
        .returning(Future.successful(HttpResponse(OK, Json.stringify(response))))
        .atLeastOnce()

      val fakeRequest = FakeRequest("POST", "/").withJsonBody(JsString("Shakti"))
      val result      = controller.claim()(fakeRequest)
      status(result)        shouldBe Status.OK
      contentAsJson(result) shouldBe response
    }

    "return 400 on POST Request with invalid JSON body" in {
      val fakeRequest = FakeRequest("POST", "/").withTextBody("""{"a"-"b"}""")
      val result      = controller.claim()(fakeRequest)
      status(result)          shouldBe BAD_REQUEST
      contentAsString(result) shouldBe "Request Body is not Json!"
    }

    "return the same status code when downstream fails on POST Request" in {
      val response = JsString("Ganesha")
      (
        httpClient
          .POST[JsValue, HttpResponse](_: String, _: JsValue, _: Seq[(String, String)])(
            _: Writes[JsValue],
            _: HttpReads[HttpResponse],
            _: HeaderCarrier,
            _: ExecutionContext
          )
        )
        .expects(*, *, *, *, *, *, *)
        .returning(Future.successful(HttpResponse(INTERNAL_SERVER_ERROR, response, Map[String, Seq[String]]().empty)))
        .atLeastOnce()

      val fakeRequest = FakeRequest("POST", "/").withJsonBody(JsString("Shakti"))
      val result      = controller.claim()(fakeRequest)
      status(result)        shouldBe INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldBe response
    }
  }
}
