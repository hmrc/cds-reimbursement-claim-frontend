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
import play.api.http.HeaderNames.ACCEPT_LANGUAGE
import play.api.i18n.Lang
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.C285ClaimRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@ImplementedBy(classOf[DefaultClaimConnector])
trait ClaimConnector {
  def submitClaim(submitClaimRequest: C285ClaimRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]
}

@Singleton
class DefaultClaimConnector @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) extends ClaimConnector
    with Logging {

  private val baseUrl: String = servicesConfig.baseUrl("cds-reimbursement-claim")

  override def submitClaim(c285ClaimRequest: C285ClaimRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {

    val submitClaimUrl: String = s"$baseUrl/cds-reimbursement-claim/claims/c285 "

    EitherT[Future, Error, HttpResponse](
      http
        .POST[C285ClaimRequest, HttpResponse](
          submitClaimUrl,
          c285ClaimRequest,
          Seq(ACCEPT_LANGUAGE -> lang.language)
        )
        .map(Right(_))
        .recover { case NonFatal(e) =>
          Left(Error(e))
        }
    )
  }
}
