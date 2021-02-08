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
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.{CDSReimbursementClaimConnector, ClaimConnector}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.Declaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[DefaultClaimService])
trait ClaimService {

  //TODO: replace with SubmitClaimRequest type
  def submitClaim(submitClaimRequest: JsValue, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, JsValue]

  def getDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Declaration]

}

@Singleton
class DefaultClaimService @Inject() (
  claimConnector: ClaimConnector,
  cdsReimbursementClaimConnector: CDSReimbursementClaimConnector
)(implicit
  ec: ExecutionContext
) extends ClaimService
    with Logging {

  def submitClaim(
    submitClaimRequest: JsValue,
    lang: Lang
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, JsValue] =
    claimConnector.submitClaim(Json.toJson(submitClaimRequest), lang).subflatMap { httpResponse =>
      if (httpResponse.status === OK)
        httpResponse
          .parseJSON[JsValue]()
          .leftMap(Error(_))
      else
        Left(
          Error(
            s"call to get submit claim came back with status ${httpResponse.status}}"
          )
        )
    }

  def getDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Declaration] =
    cdsReimbursementClaimConnector
      .getDeclarationDetails(mrn)
      .subflatMap { response =>
        if (response.status === OK)
          response
            .parseJSON[Declaration]()
            .leftMap(Error(_))
        else
          Left(Error(s"call to get declaration details ${response.status}"))
      }

}
