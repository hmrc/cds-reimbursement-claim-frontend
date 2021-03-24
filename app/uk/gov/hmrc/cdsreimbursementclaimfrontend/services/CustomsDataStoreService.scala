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
import cats.implicits.toBifunctorOps
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.mvc.Http.Status.{NOT_FOUND, OK}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.CustomsDataStoreConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Eori, Error, VerifiedEmail}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[DefaultCustomsDataStoreService])
trait CustomsDataStoreService {
  def getEmailByEori(eori: Eori)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[VerifiedEmail]]
}

@Singleton
class DefaultCustomsDataStoreService @Inject() (
  customsDataStoreConnector: CustomsDataStoreConnector
)(implicit
  ec: ExecutionContext
) extends CustomsDataStoreService
    with Logging {

  def getEmailByEori(eori: Eori)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[VerifiedEmail]] =
    customsDataStoreConnector
      .getCustomsEmail(eori)
      .subflatMap { response =>
        response.status match {
          case OK        =>
            response
              .parseJSON[VerifiedEmail]()
              .map(Some(_))
              .leftMap { error =>
                logger.warn(s"Error Parsing customs data store response: $error")
                Error(error)
              }
          case NOT_FOUND =>
            Right(None)
          case s: Int    =>
            val errorMessage = s"Customs Data Store status: $s, body: ${response.body}"
            logger.warn(errorMessage)
            Left(Error(errorMessage))
        }
      }
}
