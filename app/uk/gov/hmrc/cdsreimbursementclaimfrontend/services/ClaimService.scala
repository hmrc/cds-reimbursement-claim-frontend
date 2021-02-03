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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import cats.data.EitherT
import cats.implicits.{catsSyntaxEq, toBifunctorOps}
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.OK
import play.api.i18n.Lang
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.ClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, SubmitClaimRequest, SubmitClaimResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[ClaimServiceImpl])
trait ClaimService {

  def submitClaim(submitClaimRequest: SubmitClaimRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, SubmitClaimResponse]

}

@Singleton
class ClaimServiceImpl @Inject() (
  connector: ClaimConnector
)(implicit
  ec: ExecutionContext
) extends ClaimService
    with Logging {

  def submitClaim(
    submitClaimRequest: SubmitClaimRequest,
    lang: Lang
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubmitClaimResponse] =
    //TODO: remove serialisation here
    connector.submitClaim(Json.toJson(submitClaimRequest), lang).subflatMap { httpResponse =>
      if (httpResponse.status === OK)
        httpResponse
          .parseJSON[SubmitClaimResponse]()
          .leftMap(Error(_))
      else
        Left(
          Error(
            s"Call to get submit return came back with status ${httpResponse.status}}"
          )
        )
    }

}
