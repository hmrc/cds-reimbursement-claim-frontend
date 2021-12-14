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
import play.api.http.Status._
import play.api.i18n.Lang
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.{CDSReimbursementClaimConnector, ClaimConnector}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request.{BarsBusinessAssessRequest, BarsPersonalAssessRequest}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.{C285ClaimRequest, SubmitClaimResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[DefaultClaimService])
trait ClaimService {

  def submitClaim(submitClaimRequest: C285ClaimRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, SubmitClaimResponse]

  def getDisplayDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[DisplayDeclaration]]

  def getBusinessAccountReputation(
    barsRequest: BarsBusinessAssessRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, CommonBarsResponse]

  def getPersonalAccountReputation(
    barsRequest: BarsPersonalAssessRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, CommonBarsResponse]

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
    submitClaimRequest: C285ClaimRequest,
    lang: Lang
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubmitClaimResponse] =
    claimConnector.submitClaim(submitClaimRequest, lang).subflatMap { httpResponse =>
      if (httpResponse.status === OK)
        httpResponse
          .parseJSON[SubmitClaimResponse]()
          .leftMap(Error(_))
      else
        Left(
          Error(
            s"call to get submit claim came back with status ${httpResponse.status}}"
          )
        )
    }

  def getDisplayDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[DisplayDeclaration]] =
    cdsReimbursementClaimConnector
      .getDeclaration(mrn)
      .subflatMap { response =>
        if (response.status === OK) {
          response
            .parseJSON[DisplayDeclaration]()
            .map(Some(_))
            .leftMap(Error(_))
        } else if (response.status === NO_CONTENT) {
          Right(None)
        } else
          Left(Error(s"call to get declaration details ${response.status}"))
      }

  def getBusinessAccountReputation(
    barsRequest: BarsBusinessAssessRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, CommonBarsResponse] =
    getReputation[BarsBusinessAssessRequest, BusinessCompleteResponse](
      barsRequest,
      cdsReimbursementClaimConnector.getBusinessReputation
    )

  def getPersonalAccountReputation(
    barsRequest: BarsPersonalAssessRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, CommonBarsResponse] =
    getReputation[BarsPersonalAssessRequest, PersonalCompleteResponse](
      barsRequest,
      cdsReimbursementClaimConnector.getPersonalReputation
    )

  def getReputation[I, O <: ReputationResult](data: I, method: JsValue => EitherT[Future, Error, HttpResponse])(implicit
    writes: Writes[I],
    read: Reads[O]
  ): EitherT[Future, Error, CommonBarsResponse] =
    method(Json.toJson(data))
      .subflatMap { response =>
        response.status match {
          case OK          =>
            response.parseJSON[O]().map(_.toCommonResponse()).leftMap(Error(_))
          case BAD_REQUEST =>
            response.parseJSON[ReputationErrorResponse]().map(_.toCommonResponse()).leftMap(Error(_))
          case status: Int =>
            Left(
              Error(
                s"Call to Business Reputation Service (BARS) failed with: $status"
              )
            )
        }
      }
}
