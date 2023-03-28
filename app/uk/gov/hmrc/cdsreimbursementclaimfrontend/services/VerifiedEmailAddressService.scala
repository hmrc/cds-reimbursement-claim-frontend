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

import cats.implicits.toBifunctorOps
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import com.google.inject.Singleton
import play.mvc.Http.Status.NOT_FOUND
import play.mvc.Http.Status.OK
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.VerifiedEmailAddressConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.CdsVerifiedEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DefaultVerifiedEmailAddressService])
trait VerifiedEmailAddressService {
  def getVerifiedEmailAddress(eori: Eori)(implicit hc: HeaderCarrier): Future[Either[Error, Option[CdsVerifiedEmail]]]
}

@Singleton
class DefaultVerifiedEmailAddressService @Inject() (
  verifiedEmailAddressConnector: VerifiedEmailAddressConnector,
  sessionStore: SessionCache
)(implicit
  ec: ExecutionContext
) extends VerifiedEmailAddressService
    with Logging {

  def getVerifiedEmailAddress(eori: Eori)(implicit hc: HeaderCarrier): Future[Either[Error, Option[CdsVerifiedEmail]]] =
    sessionStore
      .updateF { sessionData =>
        if (sessionData.verifiedEmail.isDefined)
          Future.successful(Right(sessionData))
        else
          verifiedEmailAddressConnector
            .getVerifiedEmailAddress(eori)
            .subflatMap { response =>
              response.status match {
                case OK        =>
                  response
                    .parseJSON[CdsVerifiedEmail]()
                    .map(verifiedEmail => sessionData.copy(verifiedEmail = Some(verifiedEmail)))
                    .leftMap { error =>
                      logger.warn(s"Error Parsing customs data store response: $error")
                      Error(error)
                    }
                case NOT_FOUND =>
                  Right(sessionData)
                case s: Int    =>
                  val errorMessage = s"Customs Data Store status: $s, body: ${response.body}"
                  logger.warn(errorMessage)
                  Left(Error(errorMessage))
              }
            }
            .value
      }
      .map(_.map(_.verifiedEmail))

}
