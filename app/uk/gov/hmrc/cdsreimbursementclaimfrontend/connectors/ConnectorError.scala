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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CdsError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error

sealed trait ConnectorError {
  def message: String
  def throwable: Option[Throwable]
}
object ConnectorError {
  implicit def cdsError: CdsError[ConnectorError] =
    new CdsError[ConnectorError] {
      def message(error: ConnectorError): String = error.message
    }

  final case class ConnectorFailure(message: String, throwable: Option[Throwable] = None) extends ConnectorError
  final case class TechnicalServiceError(message: String, throwable: Option[Throwable] = None) extends ConnectorError
  final case class ServiceUnavailableError(message: String, throwable: Option[Throwable] = None) extends ConnectorError
}
