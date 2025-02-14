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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.config

import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Request
import play.api.mvc.RequestHeader
import play.api.mvc.Result
import play.twirl.api.Html
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.error_template
import uk.gov.hmrc.play.bootstrap.frontend.http.FrontendErrorHandler

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ErrorHandler @Inject() (
  val messagesApi: MessagesApi,
  error_template: error_template
)(implicit
  val appConfig: ViewConfig,
  val ec: ExecutionContext
) extends FrontendErrorHandler {

  override def standardErrorTemplate(
    pageTitle: String,
    heading: String,
    message: String
  )(implicit
    requestHeader: RequestHeader
  ): Future[Html] = {
    implicit val r: Request[?] = Request(requestHeader, "")
    Future.successful(error_template(pageTitle, heading, message))
  }

  def errorResult[R <: Request[?]](
  )(implicit request: R): Result =
    InternalServerError(
      error_template(
        Messages("global.error.InternalServerError500.title"),
        Messages("global.error.InternalServerError500.heading"),
        Messages("global.error.InternalServerError500.message")
      )
    )

}
