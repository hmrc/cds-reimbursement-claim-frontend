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

import javax.inject.Inject
import javax.inject.Singleton

@Singleton
class FileUploadConfig @Inject() (config: Configuration) {

  def readUpscanInitServiceProtocol: String =
    config.underlying.getString("microservice.services.upscan-initiate.protocol")

  def readUpscanInitServiceHost: String =
    config.underlying.getString("microservice.services.upscan-initiate.host")

  def readUpscanInitServicePort: String =
    config.underlying.getString("microservice.services.upscan-initiate.port")

  def readMaxFileSize(uploadDocumentKey: String): Long =
    config.underlying.getLong(s"microservice.services.upscan-initiate.$uploadDocumentKey.max-file-size")

  def readMaxFileSizeHumanReadable(uploadDocumentKey: String): String =
    s"${humanReadableFileSize(config.underlying.getLong(s"microservice.services.upscan-initiate.$uploadDocumentKey.max-file-size"))}"

  def readMaxUploadsValue(uploadDocumentKey: String): Int =
    config.underlying.getInt(s"microservice.services.upscan-initiate.$uploadDocumentKey.max-uploads")

  private def humanReadableFileSize(bytes: Long): String =
    if (bytes >= 1000000000) s"${(bytes / 1000000000)} GB"
    else if (bytes >= 1000000) s"${(bytes / 1000000)} MB"
    else if (bytes >= 1000) s"${(bytes / 1000)} kB"
    else s"$bytes B"
}
