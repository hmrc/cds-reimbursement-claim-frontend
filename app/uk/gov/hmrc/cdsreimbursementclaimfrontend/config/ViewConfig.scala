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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Inject
import javax.inject.Singleton

@Singleton
class ViewConfig @Inject() (
  val config: Configuration,
  servicesConfig: ServicesConfig,
  timeoutDialogConfig: uk.gov.hmrc.hmrcfrontend.config.TimeoutDialogConfig
) {

  private def getString(key: String): String = servicesConfig.getString(key)

  val en: String            = "en"
  val cy: String            = "cy"
  val defaultLanguage: Lang = Lang(en)

  val selfBaseUrl: String = getString("self.url")

  val viewUploadUrl: String = getString("external-url.customs-view-and-amend")

  val homePageUrl: String = selfBaseUrl + getString("home-page")

  val ggCreateAccountUrl: String = "/bas-gateway?accountType=individual&continueUrl=" +
    "%2Fclaim-back-import-duty-vat%2Fstart&origin=cds-reimbursement-claim-frontend"

  val signInUrl: String =
    getString("bas-gateway.signInUrl")

  private val signOutUrl: String =
    getString("bas-gateway.signOutUrl")

  val authLoginStubSignInUrl: String =
    getString("auth-login-stub.signInUrl")

  val ggTimeoutSeconds: Int = timeoutDialogConfig.timeoutInSeconds

  val ggCountdownSeconds: Int = timeoutDialogConfig.countdownInSeconds

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

  val govUkUrl: String                      = getString("external-url.gov-uk")
  val govUkLandingPageUrl: String           = getString("external-url.gov-uk-landing-page")
  val govUkSecuritiesLandingPageUrl: String = getString("external-url.gov-uk-securities-landing-page")

  val enableLanguageSwitching: Boolean = servicesConfig.getBoolean("enable-language-switching")

  private val contactFormServiceIdentifier = "CDSRC"

  def pageTitleWithServiceNameAndError(
    pageTitle: String,
    serviceName: String,
    errorPrefix: String,
    hasErrors: Boolean
  ): String =
    if hasErrors then s"$errorPrefix ${pageTitleWithServiceName(pageTitle, serviceName)}"
    else pageTitleWithServiceName(pageTitle, serviceName)

  def pageTitleWithServiceName(
    pageTitle: String,
    serviceName: String
  ): String = {
    val pageTitleNoHTML = pageTitle.replaceAll("\\<.*?\\>", "")
    s"$pageTitleNoHTML - $serviceName - GOV.UK"
  }

  val reportAProblemPartialUrl: String =
    s"/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"

  val reportAProblemNonJSUrl: String =
    s"/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"

  val accessibilityStatementUrl: String = getString("external-url.accessibility-statement")

  val contactHmrcUrl: String = getString("external-url.contact-hmrc")

  val eoriNumberHelpUrl: String = getString("external-url.eori-number-help")

  val subscribeToCdsUrl: String = getString("external-url.subscribe-to-cds")

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
  val researchUrl: String     = getString("external-url.research")

  val legacyC285FormUrl: String = getString("external-url.c285-form")
  val ce1179FormUrl: String     = getString("external-url.ce1179-form")

  val overpaymentsOverpaidVatGuidanceUrl: String  = getString(
    "external-url.overpayments-overpaid-vat-guidance"
  )
  val rejectedGoodsOverpaidVatGuidanceUrl: String = getString(
    "external-url.rejected-goods-overpaid-vat-guidance"
  )

  val footerLinkItems: Seq[String] = config.getOptional[Seq[String]]("footerLinkItems").getOrElse(Seq())

  def languageMap: Map[String, Lang] = Map("english" -> Lang("en"), "cymraeg" -> Lang("cy"))

  def routeToSwitchLanguage: String => Call = (lang: String) =>
    baseRoutes.LanguageSwitchController.switchToLanguage(lang)

  lazy val customsEmailFrontendUrl: String = {
    val customsEmailFrontend = "customs-email-frontend"
    val startPage            =
      servicesConfig.getString(s"microservice.services.$customsEmailFrontend.start-page")

    s"${servicesConfig.baseUrl(customsEmailFrontend)}$startPage"
  }

  def getSecuritiesEmail(rfs: ReasonForSecurity): String =
    if ReasonForSecurity.nidac.contains(rfs) then "customsaccountingrepayments@hmrc.gov.uk"
    else if ReasonForSecurity.niru.contains(rfs) then "niru@hmrc.gov.uk"
    else "ntis@hmrc.gov.uk"
}
