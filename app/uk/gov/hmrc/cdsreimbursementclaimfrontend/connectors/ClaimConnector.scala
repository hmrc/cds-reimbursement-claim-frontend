/*
 * Copyright 2026 HM Revenue & Customs
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

import org.apache.pekko.actor.ActorSystem
import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.libs.ws.JsonBodyWritables.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps.*
import uk.gov.hmrc.http.HttpReads.Implicits.*
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.http.client.HttpClientV2
import java.net.URL
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class ClaimConnector[Req, Res](
  config: ClaimConnectorConfig,
  http: HttpClientV2,
  configuration: Configuration,
  override val actorSystem: ActorSystem,
  override val uploadDocumentsConnector: UploadDocumentsConnector,
  mitigation: ClaimMitigation[Req],
  exceptionFactory: String => RuntimeException
)(implicit
  ec: ExecutionContext,
  requestWrites: Writes[Req],
  responseReads: Reads[Res]
) extends Retries
    with WafErrorMitigationHelper {

  lazy val claimUrl: String =
    config.url

  lazy val retryIntervals: Seq[FiniteDuration] =
    Retries.getConfIntervals(
      config.serviceKey,
      configuration
    )

  def submitClaim(
    claimRequest: Req,
    mitigate403: Boolean
  )(implicit hc: HeaderCarrier): Future[Res] =
    retry(retryIntervals*)(shouldRetry, retryReason) {

      http
        .post(URL(claimUrl))
        .withBody(Json.toJson(claimRequest)(requestWrites))
        .transform(
          _.addHttpHeaders(
            Seq("Accept-Language" -> "en")*
          )
        )
        .execute[HttpResponse]

    }.flatMap { response =>
      if response.status == 200 then {

        response
          .parseJSON[Res]()(responseReads)
          .fold(
            error => Future.failed(exceptionFactory(error)),
            result => Future.successful(result)
          )

      } else if response.status == 403 && mitigate403 then {

        retrySubmitWithFreeTextInputAttachedAsAFile(
          claimRequest
        )

      } else {

        Future.failed(
          exceptionFactory(
            s"Request to POST $claimUrl failed because of $response ${response.body}"
          )
        )
      }
    }

  private def retrySubmitWithFreeTextInputAttachedAsAFile(
    claimRequest: Req
  )(implicit hc: HeaderCarrier): Future[Res] = {

    val (freeTexts, rebuildRequest) = mitigation.prepareForRetry(claimRequest)

    uploadFreeTextsAsSeparateFiles(freeTexts)
      .flatMap { uploadedDocuments =>
        submitClaim(
          rebuildRequest(uploadedDocuments),
          mitigate403 = false
        )
      }
  }
}
