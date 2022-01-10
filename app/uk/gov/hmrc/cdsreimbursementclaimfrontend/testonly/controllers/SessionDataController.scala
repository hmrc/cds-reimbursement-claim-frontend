/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.testonly.controllers

import play.api.libs.json.JsError
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.mvc._
import play.twirl.api.Html
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal

@Singleton
class SessionDataController @Inject() (jcc: JourneyControllerComponents)(implicit ec: ExecutionContext)
    extends FrontendController(jcc.controllerComponents) {

  private val showSessionDataAction   = routes.SessionDataController.show
  private val submitSessionDataAction = routes.SessionDataController.show

  val show: Action[AnyContent] = Action.async { implicit request =>
    jcc.sessionCache
      .get()
      .map(
        _.fold(
          error => Ok(renderPage("", Some(error.message))),
          _.fold(Ok(renderPage("", Some("session data not found"))))(sessionData =>
            Ok(renderPage(Json.prettyPrint(Json.toJson(sessionData))))
          )
        )
      )
  }

  val submit: Action[AnyContent] = Action.async { implicit request =>
    request.body.asFormUrlEncoded
      .flatMap(_.get("sessionData").flatMap(_.headOption))
      .fold(Future.successful(Redirect(showSessionDataAction))) { sessionData =>
        try {
          val json = Json.parse(sessionData)
          implicitly[Reads[SessionData]].reads(json) match {
            case JsError(errors)       =>
              Future.successful(
                Ok(
                  renderPage(
                    sessionData,
                    Some(
                      errors
                        .map { case (path, es) =>
                          s"json parsing error at <span>$path<span>: <span>${es.map(_.message).mkString(", ")}</span>"
                        }
                        .mkString("<ul><li>", "<li>", "<ul>")
                    )
                  )
                )
              )
            case JsSuccess(session, _) =>
              jcc.sessionCache
                .store(session)
                .map(
                  _.fold(
                    error => Ok(renderPage("", Some(error.message))),
                    _ => Redirect(showSessionDataAction)
                  )
                )
          }
        } catch {
          case NonFatal(e) =>
            Future.successful(Ok(renderPage(sessionData, Some(e.getMessage()))))
        }
      }

  }

  def renderPage(sessionData: String, error: Option[String] = None): Html = Html(
    s"""
    |  <html>
    |  <head>
    |  <style>
    |  body {font-family: monospace;}
    |  form {display: flex; flex-direction: column; width: 100%; height: 100%;}
    |  div {marging: 0.2em; padding: 0.5em;}
    |  textarea {flex: 1; border: 0; margin: 1em 0;}
    |  button {font-size: 1em}
    |  .error {color: red; font-weight: bold; font-size: 1em;}
    |  </style> 
    |  </head>
    |  <body>
    |    ${error.map(e => s"""<div class="error">$e</div>""".stripMargin).getOrElse("")}
    |    <form method="POST" action="${submitSessionDataAction.url}">
    |       <button>Save</button>
    |       <textarea id="sessionData" name="sessionData">$sessionData</textarea>
    |       <button id="save">Save</button>
    |    </form>
    |  </body>
    |  </html>
    """.stripMargin
  )
}
