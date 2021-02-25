/*
 * Copyright 2021 HM Revenue & Customs
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

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject}
import controllers.Assets.ACCEPT_LANGUAGE
import play.api.i18n.Lang
import play.api.libs.json.Json
import play.mvc.Http.Status
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[DefaultClaimConnector])
trait ClaimConnector {
  def submitClaim(submitClaimRequest: SubmitClaimRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]
}

@Singleton
class DefaultClaimConnector @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) extends ClaimConnector
    with Logging {

  private val baseUrl: String = servicesConfig.baseUrl("cds-reimbursement-claim")

  override def submitClaim(submitClaimRequest: SubmitClaimRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {

    val submitClaimUrl: String = s"$baseUrl/cds-reimbursement-claim/claim"

    println(s"${Json.toJson(submitClaimRequest)}")

    EitherT[Future, Error, HttpResponse](
      http
        .POST[SubmitClaimRequest, HttpResponse](
          submitClaimUrl,
          submitClaimRequest,
          Seq(ACCEPT_LANGUAGE -> lang.language)
        )
        .map[Either[Error, HttpResponse]] { response =>
          if (response.status != Status.OK) {
            logger.warn(
              s"could not submit claim: received http " +
                s"status ${response.status} and body ${response.body}"
            )
            Left(Error("could not submit claim"))
          } else
            Right(response)
        }
        .recover { case e => Left(Error(e)) }
    )
  }
}
