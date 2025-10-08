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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import cats.data.EitherT
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait GetXiEoriMixin extends ClaimBaseController {

  def modifyClaim(claim: Claim, userXiEori: UserXiEori): Claim

  def xiEoriConnector: XiEoriConnector

  def needsUserXiEoriSubmission(claim: Claim) =
    claim.needsUserXiEoriSubmission

  final def getUserXiEoriIfNeeded(claim: Claim, enabled: Boolean)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Claim] =
    if enabled && needsUserXiEoriSubmission(claim) then {
      for
        userXiEori   <-
          EitherT(
            xiEoriConnector.getXiEori
              .map(Right(_))
              .recover((e: Throwable) => Left(Error(e)))
          )
        updatedClaim <-
          EitherT.fromEither[Future](
            Right(modifyClaim(claim, userXiEori))
          )
      yield updatedClaim
    } else EitherT.fromEither[Future](Right(claim))

}
