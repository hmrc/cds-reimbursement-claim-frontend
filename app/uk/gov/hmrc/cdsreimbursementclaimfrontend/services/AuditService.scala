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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import com.google.inject.ImplementedBy
import play.api.libs.json.JsBoolean
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Writes
import play.api.mvc.Request
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions.*
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import javax.inject.Inject
import scala.concurrent.ExecutionContext

@ImplementedBy(classOf[AuditServiceImpl])
trait AuditService {
  def sendSuccessfulClaimEvent[J : Writes, O : Writes](claim: J, output: O, summary: JsObject)(implicit
    hc: HeaderCarrier,
    request: Request[?],
    ec: ExecutionContext
  ): Unit

  def sendFailedClaimEvent[J : Writes, O : Writes](claim: J, output: O, summary: JsObject)(implicit
    hc: HeaderCarrier,
    request: Request[?],
    ec: ExecutionContext
  ): Unit
}

class AuditServiceImpl @Inject() (auditConnector: AuditConnector) extends AuditService {

  final def sendSuccessfulClaimEvent[J : Writes, O : Writes](claim: J, output: O, summary: JsObject)(implicit
    hc: HeaderCarrier,
    request: Request[?],
    ec: ExecutionContext
  ): Unit = sendClaimEvent(true, claim, output, summary)

  final def sendFailedClaimEvent[J : Writes, O : Writes](claim: J, output: O, summary: JsObject)(implicit
    hc: HeaderCarrier,
    request: Request[?],
    ec: ExecutionContext
  ): Unit = sendClaimEvent(false, claim, output, summary)

  // audit event are sent in the fire-and-forget manner
  private def sendClaimEvent[J : Writes, O : Writes](success: Boolean, claim: J, output: O, summary: JsObject)(implicit
    hc: HeaderCarrier,
    request: Request[?],
    ec: ExecutionContext
  ): Unit =
    auditConnector.sendExtendedEvent(
      ExtendedDataEvent(
        auditSource = "cds-reimbursement-claim-frontend",
        auditType = "CustomsReimbursementClaim",
        detail = Json.obj(
          "success" -> JsBoolean(success),
          "summary" -> summary,
          "claim"   -> implicitly[Writes[O]].writes(output),
          "input"   -> implicitly[Writes[J]].writes(claim)
        ),
        tags = hc.toAuditTags("customs-reimbursement-claim", request.uri)
      )
    )
}
