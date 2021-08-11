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

import play.api.Configuration
import play.api.i18n.Lang
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.Duration

@Singleton
class ViewConfig @Inject() (config: Configuration, servicesConfig: ServicesConfig) {

  private def getString(key: String): String     = servicesConfig.getString(key)
  private def getDuration(key: String): Duration = servicesConfig.getDuration(key)

  val en: String            = "en"
  val cy: String            = "cy"
  val defaultLanguage: Lang = Lang(en)

  val ggCreateAccountUrl: String = "/bas-gateway?accountType=individual&continueUrl=" +
    "%2Fclaim-for-reimbursement-of-import-duties%2Fstart&origin=cds-reimbursement-claim-frontend"

  val signOutUrl: URL = new URL(getString("bas-gateway.signOutUrl"))

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

  val serviceFeedBackUrl: String = config.get[String]("microservice.services.feedback.url") +
    config.get[String]("microservice.services.feedback.source")

  val feedbackSignOut: String = signOutUrl + s"?continue=$serviceFeedBackUrl"

  val govUkUrl: String = getString("external-url.gov-uk")

  val enableLanguageSwitching: Boolean = servicesConfig.getBoolean("enable-language-switching")

  private val contactFormServiceIdentifier = "CDSRC"

  def pageTitleWithServiceName(pageTitle: String, serviceName: String, hasErrors: Boolean): String =
    if (hasErrors)
      s"ERROR: $pageTitle - $serviceName - GOV.UK"
    else
      s"$pageTitle - $serviceName - GOV.UK"

  val reportAProblemPartialUrl: String =
    s"/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"

  val reportAProblemNonJSUrl: String =
    s"/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"

  val accessibilityStatementUrl: String = getString("external-url.accessibility-statement")

  lazy val contactHmrcUrl: String = {
    val baseUrl =  servicesConfig.baseUrl("contact-frontend")
    val contactPath = servicesConfig.getString(s"microservice.services.contact-frontend.contact-hmrc-url")
    s"$baseUrl$contactPath"
  }

  val eoriNumberHelpUrl: String = getString("external-url.eori-number-help")

  val abilityNetUrl: String = getString("external-url.ability-net")

  val webStandardUrl: String = getString("external-url.web-standard")

  val taxServiceUrl: String = getString("external-url.tax-service")

  val equalityServiceUrl: String = getString("external-url.equality-service")

  val equalityOrgUrl: String = getString("external-url.equality-org")

  val contactUsUrl: String = getString("external-url.contact-us")

  val accessibilityCentreUrl: String = getString("external-url.accessibility-centre")

  val capitalGainsUrl: String = getString("external-url.capital-gains")

  val mrnGuideUrl: String = getString("external-url.mrn-guide")

  val contactCdsTeamUrl: String = getString("external-url.contact-cds-team")

  val importExportUrl: String = getString("external-url.import-export")

  val footerLinkItems: Seq[String] = config.getOptional[Seq[String]]("footerLinkItems").getOrElse(Seq())

  lazy val timeout: Int = getDuration("gg.timeout").toSeconds.toInt

  lazy val countdown: Int = getDuration("gg.countdown").toSeconds.toInt

  val selfBaseUrl: String = getString("self.url")

  def buildCompleteSelfUrl(call: Call): URL = buildCompleteSelfUrl(call.url)

  def buildCompleteSelfUrl(path: String) = new URL(s"$selfBaseUrl$path")

  def languageMap: Map[String, Lang] = Map("english" -> Lang("en"), "cymraeg" -> Lang("cy"))

  def routeToSwitchLanguage: String => Call = (lang: String) => routes.LanguageSwitchController.switchToLanguage(lang)
}
