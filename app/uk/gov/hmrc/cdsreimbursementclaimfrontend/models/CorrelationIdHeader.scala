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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import play.api.mvc.Headers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Hash

import java.util.Locale
import java.util.UUID

object CorrelationIdHeader {

  val headerName          = "X-Correlation-ID"
  val headerNameLowercase = headerName.toLowerCase(Locale.UK)

  def random(): (String, String) = (headerName, UUID.randomUUID().toString)

  def apply(headerValue: String): (String, String) = (headerName, headerValue)

  def from(eori: Eori): (String, String) =
    (headerName, Hash(eori.value).take(8) + UUID.randomUUID().toString.drop(8))

  def from(uuid: UUID): (String, String) = (headerName, uuid.toString())

  def from(eoriOpt: Option[Eori], sessionId: Option[String]): (String, String) = {
    val uuid = UUID.randomUUID().toString
    (
      headerName,
      eoriOpt
        .map(eori => Hash(eori.value))
        .getOrElse("xxxxxxxxxx")
        .take(8) +
        sessionId
          .map(_.drop(8).take(10) + uuid.drop(18))
          .getOrElse(uuid.drop(8))
    )
  }

  def from(eori: Eori, sessionId: Option[String]): (String, String) = {
    val uuid = UUID.randomUUID().toString
    (
      headerName,
      Hash(eori.value).take(8) +
        sessionId
          .map(_.drop(8).take(10) + uuid.drop(18))
          .getOrElse(uuid.drop(8))
    )
  }

  def from(eori: Eori, uuid: UUID): (String, String) =
    (headerName, Hash(eori.value).take(8) + uuid.toString.drop(8).take(10) + UUID.randomUUID().toString.drop(18))

  implicit class HeaderOps(val headers: Headers) extends AnyVal {
    def addIfMissing(newHeader: (String, String)): Headers =
      if (
        headers.keys
          .map(_.toLowerCase(Locale.UK))
          .contains(newHeader._1)
      ) headers
      else {
        headers.add(newHeader)
      }
  }

}
