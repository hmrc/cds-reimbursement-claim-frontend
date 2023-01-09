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

import javax.inject.Inject
import javax.inject.Singleton
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import scala.concurrent.duration.FiniteDuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.Retries
import play.api.Configuration

@Singleton
class UploadDocumentsConfig @Inject() (servicesConfig: ServicesConfig, configuration: Configuration) {

  lazy val baseUrl: String =
    servicesConfig.baseUrl("upload-documents-frontend")

  lazy val callbackUrlPrefix: String =
    servicesConfig.getConfString("upload-documents-frontend.callback-url-prefix", "")

  lazy val contextPath: String =
    servicesConfig.getConfString("upload-documents-frontend.context-path", "/upload-documents")

  lazy val publicUrl: String =
    servicesConfig.getConfString("upload-documents-frontend.public-url", "")

  lazy val retryIntervals: Seq[FiniteDuration] =
    Retries.getConfIntervals("upload-documents-frontend", configuration)

  lazy val initializationUrl: String = s"$baseUrl/internal/initialize"

  lazy val wipeOutUrl: String = s"$baseUrl/internal/wipe-out"
}
