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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.config

import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.{Inject, Singleton}

@Singleton
class ViewConfig @Inject() (servicesConfig: ServicesConfig) {
  private def getString(key: String): String = servicesConfig.getString(key)

  val ggCreateAccountUrl: String = "/bas-gateway?accountType=individual&continueUrl=" +
    "%2Fclaim-for-reimbursement-of-import-duties%2Fstart&origin=cds-reimbursement-claim-frontend"

  val signOutUrl: String = getString("bas-gateway.signOutUrl")

  val ggTimeoutSeconds: Long =
    servicesConfig.getDuration("gg.timeout").toSeconds

  val ggCountdownSeconds: Long =
    servicesConfig.getDuration("gg.countdown").toSeconds

  val ggKeepAliveUrl: String =
    "/claim-for-reimbursement-of-import-duties" + routes.StartController.keepAlive().url

  val ggTimedOutUrl: String =
    signOutUrl + "?continue=/claim-for-reimbursement-of-import-duties" + routes.StartController
      .timedOut()
      .url

  val ggSignOut: String =
    signOutUrl + "?continue=/claim-for-reimbursement-of-import-duties" + routes.StartController
      .start()
      .url

  val govUkUrl: String = getString("external-url.gov-uk")

  val userRecruitmentBannerEnabled: Boolean = servicesConfig.getBoolean("user-recruitment-banner.enabled")

  val userRecruitmentUrl: String = getString("user-recruitment-banner.url")

  val enableLanguageSwitching: Boolean = servicesConfig.getBoolean("enable-language-switching")

  private val contactFormServiceIdentifier = "CDSRC"

  val reportAProblemPartialUrl: String =
    s"/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"

  val reportAProblemNonJSUrl: String =
    s"/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"

  val mrnGuideUrl: String = getString("external-url.mrn-guide")
}
