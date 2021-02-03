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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.GetDeclarationConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, GetDeclarationResponse, MRN}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class GetDeclarationService @Inject() (declarationInfoConnector: GetDeclarationConnector)(implicit
  ec: ExecutionContext
) extends Logging {

  def getDeclarationInfo(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, GetDeclarationResponse] =
    declarationInfoConnector
      .getDeclarationInfo(mrn)
      .subflatMap { httpResponse =>
        if (httpResponse.status === Status.OK) {
          Try(httpResponse.json.as[GetDeclarationResponse]).fold(err => Left(Error(err)), js => Right(js))
        } else {
          Left(
            Error(
              s"Call to get declaration information came back with status ${httpResponse.status}, body: ${httpResponse.body}"
            )
          )
        }
      }
}
