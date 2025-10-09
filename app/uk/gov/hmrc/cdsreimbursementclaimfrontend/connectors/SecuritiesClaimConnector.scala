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

import cats.syntax.eq.*
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import org.apache.pekko.actor.ActorSystem
import play.api.Configuration
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps.*
import uk.gov.hmrc.http.HttpReads.Implicits.*
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.http.client.HttpClientV2
import play.api.libs.ws.JsonBodyWritables.*
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import javax.inject.Singleton
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[SecuritiesClaimConnectorImpl])
trait SecuritiesClaimConnector {
  def submitClaim(claimRequest: SecuritiesClaimConnector.Request, mitigate403: Boolean)(implicit
    hc: HeaderCarrier
  ): Future[SecuritiesClaimConnector.Response]
}
@Singleton
class SecuritiesClaimConnectorImpl @Inject() (
  http: HttpClientV2,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  val actorSystem: ActorSystem,
  val uploadDocumentsConnector: UploadDocumentsConnector
)(implicit
  ec: ExecutionContext
) extends SecuritiesClaimConnector
    with Retries
    with WafErrorMitigationHelper {

  import SecuritiesClaimConnector._

  lazy val baseUrl: String                     = servicesConfig.baseUrl("cds-reimbursement-claim")
  lazy val contextPath: String                 =
    servicesConfig.getConfString("cds-reimbursement-claim.context-path", "cds-reimbursement-claim")
  lazy val claimUrl: String                    = s"$baseUrl$contextPath/claims/securities"
  lazy val retryIntervals: Seq[FiniteDuration] = Retries.getConfIntervals("cds-reimbursement-claim", configuration)

  def submitClaim(claimRequest: Request, mitigate403: Boolean)(implicit
    hc: HeaderCarrier
  ): Future[Response] =
    retry(retryIntervals*)(shouldRetry, retryReason)(
      http
        .post(URL(claimUrl))
        .withBody(Json.toJson(claimRequest))
        .transform(_.addHttpHeaders(Seq("Accept-Language" -> "en")*))
        .execute[HttpResponse]
    ).flatMap(response =>
      if response.status === 200 then
        response
          .parseJSON[Response]()
          .fold(error => Future.failed(Exception(error)), Future.successful)
      else if response.status == 403 && mitigate403
      then retrySubmitWithFreeTextInputAttachedAsAFile(claimRequest)
      else
        Future.failed(
          Exception(s"Request to POST $claimUrl failed because of $response ${response.body}")
        )
    )

  def retrySubmitWithFreeTextInputAttachedAsAFile(claimRequest: Request)(implicit
    hc: HeaderCarrier
  ): Future[Response] = {
    val (freeTexts, sanitizedClaim) = claimRequest.claim.excludeFreeTextInputs()
    uploadFreeTextsAsSeparateFiles(freeTexts)
      .flatMap(freeTextUploads =>
        submitClaim(
          Request(sanitizedClaim.copy(supportingEvidences = sanitizedClaim.supportingEvidences ++ freeTextUploads)),
          mitigate403 = false
        )
      )
  }
}

object SecuritiesClaimConnector {

  final case class Request(claim: SecuritiesClaim.Output)
  final case class Response(caseNumber: String)
  final case class Exception(msg: String) extends scala.RuntimeException(msg)

  implicit val requestFormat: Format[Request]   = Json.format[Request]
  implicit val responseFormat: Format[Response] = Json.format[Response]
}
