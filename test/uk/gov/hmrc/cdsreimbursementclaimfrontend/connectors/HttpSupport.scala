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

package uk.gov.hmrc.cdsreimbursementclaim.connectors

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Writes
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpReads, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

trait HttpSupport { this: MockFactory with Matchers ⇒

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val mockHttp: HttpClient = mock[HttpClient]

  def mockPost[A](url: String, headers: Seq[(String, String)], body: A)(result: Either[Throwable, HttpResponse]) =
    (mockHttp
      .POST(_: String, _: A, _: Seq[(String, String)])(
        _: Writes[A],
        _: HttpReads[HttpResponse],
        _: HeaderCarrier,
        _: ExecutionContext
      ))
      .expects(url, body, headers, *, *, *, *)
      .returning(
        result.fold[Future[HttpResponse]](Future.failed, Future.successful)
      )

}
