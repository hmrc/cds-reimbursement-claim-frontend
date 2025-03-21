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

import cats.data.EitherT
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExistingClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HttpReads.Implicits.*
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DefaultDeclarationConnector])
trait DeclarationConnector {

  def getDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def getDeclaration(mrn: MRN, reasonForSecurity: ReasonForSecurity)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def getIsDuplicate(
    mrn: MRN,
    reason: ReasonForSecurity
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, ExistingClaim]
}

@Singleton
class DefaultDeclarationConnector @Inject() (http: HttpClientV2, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) extends DeclarationConnector
    with Logging {

  private val baseUrl: String = servicesConfig.baseUrl("cds-reimbursement-claim")

  override def getDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    val getDeclarationUrl = s"$baseUrl/cds-reimbursement-claim/declaration/${mrn.value}"

    EitherT[Future, Error, HttpResponse](
      http
        .get(URL(getDeclarationUrl))
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }

  override def getDeclaration(mrn: MRN, reasonForSecurity: ReasonForSecurity)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {
    val getDeclarationUrl =
      s"$baseUrl/cds-reimbursement-claim/declaration/${mrn.value}/reason-for-security?reasonForSecurity=$reasonForSecurity"

    EitherT[Future, Error, HttpResponse](
      http
        .get(URL(getDeclarationUrl))
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }

  override def getIsDuplicate(
    mrn: MRN,
    reason: ReasonForSecurity
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, ExistingClaim] = {
    val getDeclarationUrl =
      s"$baseUrl/cds-reimbursement-claim/declaration/${mrn.value}/claim-exists?reasonForSecurity=$reason"

    EitherT[Future, Error, ExistingClaim](
      http
        .get(URL(getDeclarationUrl))
        .execute[ExistingClaim]
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }
}
