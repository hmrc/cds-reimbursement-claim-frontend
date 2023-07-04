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

import play.api.Configuration
import play.api.i18n.Lang
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.duration.Duration

@Singleton
class ViewConfig @Inject() (config: Configuration, servicesConfig: ServicesConfig) {

  private def getString(key: String): String     = servicesConfig.getString(key)
  private def getDuration(key: String): Duration = servicesConfig.getDuration(key)

  val en: String            = "en"
  val cy: String            = "cy"
  val defaultLanguage: Lang = Lang(en)

  val selfBaseUrl: String = getString("self.url")

  val viewUploadUrl: String = getString("external-url.customs-view-and-amend")

  val homePageUrl: String = selfBaseUrl + getString("home-page")

  val ggCreateAccountUrl: String = "/bas-gateway?accountType=individual&continueUrl=" +
    "%2Fclaim-back-import-duty-vat%2Fstart&origin=cds-reimbursement-claim-frontend"

  private val signOutUrl: String =
    getString("bas-gateway.signOutUrl")

  val ggTimeoutSeconds: Long =
    servicesConfig.getDuration("gg.timeout").toSeconds

  val ggCountdownSeconds: Long =
    servicesConfig.getDuration("gg.countdown").toSeconds

  val ggKeepAliveUrl: String =
    s"$selfBaseUrl/claim-back-import-duty-vat" + baseRoutes.StartController.keepAlive().url

  val ggTimedOutUrl: String =
    signOutUrl + s"?continue=$selfBaseUrl/claim-back-import-duty-vat" + baseRoutes.StartController
      .timedOut()
      .url

  val signOutPage: String =
    s"$selfBaseUrl/claim-back-import-duty-vat/sign-out"

  val ggSignOut: String =
    signOutUrl + s"?continue=$signOutPage"

  val weSignedYouOutPageUrl: String =
    s"$selfBaseUrl/claim-back-import-duty-vat${baseRoutes.StartController.timedOut().url}"

  val govUkUrl: String = getString("external-url.gov-uk")

  val enableLanguageSwitching: Boolean = servicesConfig.getBoolean("enable-language-switching")

  private val contactFormServiceIdentifier = "CDSRC"

  def pageTitleWithServiceName(pageTitle: String, serviceName: String, hasErrors: Boolean): String = {
    val pageTitleNoHTML = pageTitle.replaceAll("\\<.*?\\>", "")
    if (hasErrors)
      s"Error: $pageTitleNoHTML - $serviceName - GOV.UK"
    else
      s"$pageTitleNoHTML - $serviceName - GOV.UK"
  }

  val reportAProblemPartialUrl: String =
    s"/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"

  val reportAProblemNonJSUrl: String =
    s"/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"

  val accessibilityStatementUrl: String = getString("external-url.accessibility-statement")

  val contactHmrcUrl: String = getString("external-url.contact-hmrc")

  val eoriNumberHelpUrl: String = getString("external-url.eori-number-help")

  val scheduledUploadTemplateUrl: String = getString("external-url.scheduled-upload-template")

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

  val bod3FormUrl: String = getString("external-url.bod3-form")

  val bod4FormUrl: String = getString("external-url.bod4-form")

  val importingGoodsIntoTheUk: String = getString("external-url.importing-goods-into-the-uk")

  val ukTradeTariffGuidance: String = getString("external-url.uk-trade-tariff-guidance")

  val betaFeedbackUrl: String = getString("external-url.beta-feedback")
  val feedbackUrl: String     = getString("external-url.feedback")

  val footerLinkItems: Seq[String] = config.getOptional[Seq[String]]("footerLinkItems").getOrElse(Seq())

  lazy val timeout: Int = getDuration("gg.timeout").toSeconds.toInt

  lazy val countdown: Int = getDuration("gg.countdown").toSeconds.toInt

  def languageMap: Map[String, Lang] = Map("english" -> Lang("en"), "cymraeg" -> Lang("cy"))

  def routeToSwitchLanguage: String => Call = (lang: String) =>
    baseRoutes.LanguageSwitchController.switchToLanguage(lang)

  lazy val customsEmailFrontendUrl: String = {
    val customsEmailFrontend = "customs-email-frontend"
    val startPage            =
      servicesConfig.getString(s"microservice.services.$customsEmailFrontend.start-page")

    s"${servicesConfig.baseUrl(customsEmailFrontend)}$startPage"
  }
}
