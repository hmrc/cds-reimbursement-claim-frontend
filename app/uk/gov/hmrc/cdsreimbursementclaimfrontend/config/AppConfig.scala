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

import play.api.{Configuration, Environment, Mode}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import javax.inject.{Inject, Singleton}
import cats.instances.string._
import cats.kernel.Eq
import cats.syntax.eq._

@Singleton
class AppConfig @Inject() (val config: Configuration, val environment: Environment, servicesConfig: ServicesConfig) {
  val appName: String              = servicesConfig.getString("appName")
  val footerLinkItems: Seq[String] = config.getOptional[Seq[String]]("footerLinkItems").getOrElse(Seq())
  lazy val registerCdsUrl          = config.get[String]("microservice.services.cds-reimbursement-claim-frontend.cdsRegisterUrl")
  lazy val subscribeCdsUrl         =
    config.get[String]("microservice.services.cds-reimbursement-claim-frontend.cdsSubscribeUrl")

  val claimsEndpoint: String = servicesConfig.baseUrl("cds-reimbursement-claim") + servicesConfig.getConfString(
    "cds-reimbursement-claim.claims-url",
    "Undefined"
  )

  val runMode: Option[String]                        = config.getOptional[String]("run.mode")
  implicit private val modeEquals: Eq[play.api.Mode] = Eq.fromUniversalEquals
  val isDevEnv: Boolean                              = if (environment.mode === Mode.Test) false else runMode.forall(_ === Mode.Dev.toString)
}
