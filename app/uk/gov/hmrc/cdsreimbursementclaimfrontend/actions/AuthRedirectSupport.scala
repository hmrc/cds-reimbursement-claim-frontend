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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.actions

import javax.inject.Inject
import play.api.mvc.{AnyContent, Request, Result}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core.NoActiveSession
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AppConfig
import uk.gov.hmrc.play.bootstrap.config.AuthRedirects

class AuthRedirectSupport @Inject() (appConfig: AppConfig) extends AuthRedirects {

  override lazy val config: Configuration = appConfig.config
  override lazy val env: Environment      = appConfig.environment

  def withAuthRecovery(implicit request: Request[AnyContent]): PartialFunction[Throwable, Result] = {
    case _: NoActiveSession => toGGLogin(continueUrl = request.path)
  }
}
