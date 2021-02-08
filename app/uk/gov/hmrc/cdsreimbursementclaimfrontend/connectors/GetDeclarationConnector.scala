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
import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AppConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, MRN}
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GetDeclarationConnector @Inject() (http: HttpClient, val appConfig: AppConfig)(implicit ec: ExecutionContext) {

  def getDeclarationInfo(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    val url = appConfig.declarationInfoEndpoint + "/" + mrn.value
    EitherT[Future, Error, HttpResponse](
      http
        .GET[HttpResponse](url)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }

}
