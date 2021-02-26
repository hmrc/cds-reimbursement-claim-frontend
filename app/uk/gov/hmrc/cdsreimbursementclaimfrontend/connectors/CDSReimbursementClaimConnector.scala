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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import javax.inject.Singleton
import play.api.libs.json.JsValue

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[DefaultCDSReimbursementClaimConnector])
trait CDSReimbursementClaimConnector {
  def getDeclarationDetails(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def getBusinessReputation(data: JsValue)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]
  def getPersonalReputation(data: JsValue)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

}

@Singleton
class DefaultCDSReimbursementClaimConnector @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) extends CDSReimbursementClaimConnector
    with Logging {

  private val baseUrl: String = servicesConfig.baseUrl("cds-reimbursement-claim")

  override def getDeclarationDetails(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    val getDeclarationUrl = s"$baseUrl/cds-reimbursement-claim/declaration/${mrn.value}"

    EitherT[Future, Error, HttpResponse](
      http
        .GET[HttpResponse](getDeclarationUrl)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }

  def getBusinessReputation(data: JsValue)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    val url = getUri("bank-account-reputation", "business")
    getReputation(data, url)
  }

  def getPersonalReputation(data: JsValue)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    val url = getUri("bank-account-reputation", "personal")
    getReputation(data, url)
  }

  def getUri(serviceName: String, apiName: String): String =
    servicesConfig.baseUrl(serviceName) + servicesConfig.getString(s"microservice.services.$serviceName.$apiName")

  def getReputation(data: JsValue, url: String)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .POST[JsValue, HttpResponse](url, data)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

}
