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

import cats.data.EitherT
import cats.implicits.catsSyntaxEq
import cats.implicits.toBifunctorOps
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.http.Status._
import play.api.i18n.Lang
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.DeclarationConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.GetDeclarationError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DefaultClaimService])
trait ClaimService {
  def getDisplayDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[DisplayDeclaration]]

  def getDisplayDeclaration(mrn: MRN, rfs: ReasonForSecurity)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[DisplayDeclaration]]

  def getDisplayDeclarationWithErrorCodes(mrn: MRN, reasonForSecurity: ReasonForSecurity)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, GetDeclarationError, DisplayDeclaration]
}

@Singleton
class DefaultClaimService @Inject() (
  DeclarationConnector: DeclarationConnector
)(implicit
  ec: ExecutionContext
) extends ClaimService
    with Logging {

  def getDisplayDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[DisplayDeclaration]] =
    DeclarationConnector
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

  def getDisplayDeclaration(mrn: MRN, reasonForSecurity: ReasonForSecurity)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[DisplayDeclaration]] =
    DeclarationConnector
      .getDeclaration(mrn, reasonForSecurity)
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

  def getDisplayDeclarationWithErrorCodes(mrn: MRN, reasonForSecurity: ReasonForSecurity)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, GetDeclarationError, DisplayDeclaration] =
    DeclarationConnector
      .getDeclaration(mrn, reasonForSecurity)
      .leftMap(_ => GetDeclarationError.unexpectedError)
      .subflatMap { response =>
        if (response.status === OK) {
          response
            .parseJSON[DisplayDeclaration]()
            .leftMap(_ => GetDeclarationError.unexpectedError)
        } else if (response.status === BAD_REQUEST) {
          Left(
            response
              .parseJSON[GetDeclarationError]()
              .toOption
              .getOrElse(GetDeclarationError.unexpectedError)
          )
        } else {
          Left(GetDeclarationError.unexpectedError)
        }
      }
}
