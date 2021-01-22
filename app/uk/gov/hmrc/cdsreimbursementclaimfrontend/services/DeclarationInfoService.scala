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
import cats.syntax.eq._
import javax.inject.{Inject, Singleton}
import play.api.http.Status
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaim.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.DeclarationInfoConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarationInfoResponse._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarationInfoRequest, DeclarationInfoResponse, Error}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class DeclarationInfoService @Inject() (declarationInfoConnector: DeclarationInfoConnector)(implicit
  ec: ExecutionContext
) extends Logging {

  def getDeclarationInfo(
    declarationId: String
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, DeclarationInfoResponse] =
    declarationInfoConnector
      .getDeclarationInfo(Json.toJson(DeclarationInfoRequest(declarationId)))
      .subflatMap { httpResponse =>
        if (httpResponse.status === Status.OK) {
          Try(httpResponse.json.as[DeclarationInfoResponse]).fold(err => Left(Error(err)), js => Right(js))
        } else {
          Left(
            Error(
              s"Call to get declaration information came back with status ${httpResponse.status}, body: ${httpResponse.body}"
            )
          )
        }
      }
}
