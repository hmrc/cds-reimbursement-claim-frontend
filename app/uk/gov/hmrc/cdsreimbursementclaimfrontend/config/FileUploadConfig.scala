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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.config

import configs.ConfigReader
import configs.syntax._
import play.api.Configuration

import javax.inject.Inject
import javax.inject.Singleton

@Singleton
class FileUploadConfig @Inject() (config: Configuration) {

  def readUpscanInitServiceProtocol: String =
    getUpscanInitiateConfig[String]("protocol")

  def readUpscanInitServiceHost: String =
    getUpscanInitiateConfig[String]("host")

  def readUpscanInitServicePort: String =
    getUpscanInitiateConfig[String]("port")

  def readMaxFileSize(uploadDocumentKey: String): Long =
    getUpscanInitiateConfig[Long](s"$uploadDocumentKey.max-file-size")

  def readMaxUploadsValue(uploadDocumentKey: String): Int =
    getUpscanInitiateConfig[Int](s"$uploadDocumentKey.max-uploads")

  private def getUpscanInitiateConfig[A : ConfigReader](key: String): A =
    config.underlying
      .get[A](s"microservice.services.upscan-initiate.$key")
      .value
}
