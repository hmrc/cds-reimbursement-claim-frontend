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

import com.google.inject.Inject
import play.api.i18n.{I18nSupport, Lang, MessagesApi}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

class LanguageSwitchController @Inject() (
  viewConfig: ViewConfig,
  override implicit val messagesApi: MessagesApi,
  val controllerComponents: MessagesControllerComponents
) extends FrontendBaseController
    with I18nSupport {

  private def fallbackURL: String = "https://www.gov.uk/government/organisations/hm-revenue-customs"

  def switchToLanguage(language: String): Action[AnyContent] = Action { implicit request =>
    val languageToUse =
      if (viewConfig.enableLanguageSwitching) {
        Lang(language)
      } else {
        Lang("en")
      }

    val redirectURL = request.headers.get(REFERER).getOrElse(fallbackURL)
    Redirect(redirectURL).withLang(languageToUse)
  }
}
