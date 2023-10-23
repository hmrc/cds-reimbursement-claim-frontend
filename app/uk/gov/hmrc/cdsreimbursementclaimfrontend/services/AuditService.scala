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
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Writes
import play.api.mvc.Request
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import javax.inject.Inject
import scala.annotation.nowarn
import scala.concurrent.ExecutionContext
import play.api.libs.json.JsBoolean

@ImplementedBy(classOf[AuditServiceImpl])
trait AuditService {
  def sendSuccessfulClaimEvent[J : Writes, O : Writes](journey: J, output: O, summary: JsObject)(implicit
    hc: HeaderCarrier,
    request: Request[_],
    ec: ExecutionContext
  ): Unit

  def sendFailedClaimEvent[J : Writes, O : Writes](journey: J, output: O, summary: JsObject)(implicit
    hc: HeaderCarrier,
    request: Request[_],
    ec: ExecutionContext
  ): Unit
}

class AuditServiceImpl @Inject() (auditConnector: AuditConnector) extends AuditService {

  final def sendSuccessfulClaimEvent[J : Writes, O : Writes](journey: J, output: O, summary: JsObject)(implicit
    hc: HeaderCarrier,
    request: Request[_],
    ec: ExecutionContext
  ): Unit = sendClaimEvent(true, journey, output, summary)

  final def sendFailedClaimEvent[J : Writes, O : Writes](journey: J, output: O, summary: JsObject)(implicit
    hc: HeaderCarrier,
    request: Request[_],
    ec: ExecutionContext
  ): Unit = sendClaimEvent(false, journey, output, summary)

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  @nowarn // audit event are sent in the fire-and-forget manner
  private def sendClaimEvent[J : Writes, O : Writes](success: Boolean, journey: J, output: O, summary: JsObject)(
    implicit
    hc: HeaderCarrier,
    request: Request[_],
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
          "input"   -> implicitly[Writes[J]].writes(journey)
        ),
        tags = hc.toAuditTags("customs-reimbursement-claim", request.uri)
      )
    )
}
