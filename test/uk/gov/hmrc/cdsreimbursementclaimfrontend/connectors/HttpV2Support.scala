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

import izumi.reflect.Tag
import org.scalamock.handlers.CallHandler2
import org.scalamock.handlers.CallHandler4
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.JsValue
import play.api.libs.ws.BodyWritable
import play.api.libs.ws.WSRequest
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import uk.gov.hmrc.http.*
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.client.RequestBuilder

import java.net.URL
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.annotation.nowarn

@nowarn
trait HttpV2Support { this: MockFactory & Matchers =>

  implicit val hc: HeaderCarrier                            = HeaderCarrier()
  implicit val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  val mockHttp: HttpClientV2             = mock[HttpClientV2]
  val mockRequestBuilder: RequestBuilder = mock[RequestBuilder]

  def mockHttpPostSuccess[A](url: String, requestBody: JsValue, hasHeaders: Boolean = false)(response: A) = {
    mockHttpPost(URL(url)).once()
    mockRequestBuilderWithBody(requestBody).once()
    if hasHeaders then mockRequestBuilderTransform().once()
    mockRequestBuilderExecuteWithoutException(response).once()
  }

  def mockHttpPostWithException(
    url: String,
    requestBody: JsValue,
    hasHeaders: Boolean = false
  )(exception: Exception) = {
    mockHttpPost(URL(url))
    mockRequestBuilderWithBody(requestBody)
    if hasHeaders then mockRequestBuilderTransform()
    mockRequestBuilderExecuteWithException(exception)
  }

  def mockHttpPostStringSuccess[A](url: String, requestBody: String)(response: A) = {
    mockHttpPost(URL(url))
    mockRequestBuilderWithString(requestBody)
    mockRequestBuilderExecuteWithoutException(response)
  }

  def mockHttpPostStringWithException(url: String, requestBody: String, exception: Exception) = {
    mockHttpPost(URL(url))
    mockRequestBuilderWithString(requestBody)
    mockRequestBuilderTransform()
    mockRequestBuilderExecuteWithException(exception)
  }

  def mockHttpGetSuccess[A](
    url: URL
  )(response: A) = {
    mockHttpGet[A](url)
    mockRequestBuilderExecuteWithoutException[A](response)
  }

  def mockHttpGetFailure(
    url: URL
  )(exception: Exception) = {
    mockHttpGet(url)
    mockRequestBuilderExecuteWithException(exception)
  }

  private def mockHttpGet[A](url: URL) =
    (mockHttp
      .get(_: URL)(_: HeaderCarrier))
      .expects(url, *)
      .returning(mockRequestBuilder)

  def mockHttpPost[A](url: URL) =
    (mockHttp
      .post(_: URL)(_: HeaderCarrier))
      .expects(url, *)
      .returning(mockRequestBuilder)

  def mockRequestBuilderWithBody[JsValue](
    body: JsValue
  ): CallHandler4[JsValue, BodyWritable[JsValue], Tag[JsValue], ExecutionContext, RequestBuilder] =
    (mockRequestBuilder
      .withBody(_: JsValue)(_: BodyWritable[JsValue], _: Tag[JsValue], _: ExecutionContext))
      .expects(body, *, *, *)
      .returning(mockRequestBuilder)

  def mockRequestBuilderWithString(
    body: String
  ) =
    (mockRequestBuilder
      .withBody(_: String)(_: BodyWritable[String], _: Tag[String], _: ExecutionContext))
      .expects(body, *, *, *)
      .returning(mockRequestBuilder)

  def mockRequestBuilderExecuteWithoutException[A](
    value: A
  ): CallHandler2[HttpReads[A], ExecutionContext, Future[A]] =
    (mockRequestBuilder
      .execute(_: HttpReads[A], _: ExecutionContext))
      .expects(*, *)
      .returning(Future successful value)

  def mockRequestBuilderTransform() =
    (mockRequestBuilder
      .transform(_: WSRequest => WSRequest))
      .expects(*)
      .returning(mockRequestBuilder)

  def mockRequestBuilderExecuteWithException[A](
    ex: Exception
  ) =
    (mockRequestBuilder
      .execute(_: HttpReads[A], _: ExecutionContext))
      .expects(*, *)
      .returning(Future failed ex)

}
