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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import com.google.inject.ImplementedBy
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

@ImplementedBy(classOf[SecuritiesClaimConnectorImpl])
trait SecuritiesClaimConnector {
  def submitClaim(claimRequest: SecuritiesClaimConnector.Request, mitigate403: Boolean)(implicit
    hc: HeaderCarrier
  ): Future[SecuritiesClaimConnector.Response]
}

object SecuritiesClaimConnector {

  final case class Request(claim: SecuritiesClaim.Output)
  final case class Response(caseNumber: String)
  final case class Exception(msg: String) extends scala.RuntimeException(msg)

  implicit val requestFormat: Format[Request]   = Json.format[Request]
  implicit val responseFormat: Format[Response] = Json.format[Response]
}
