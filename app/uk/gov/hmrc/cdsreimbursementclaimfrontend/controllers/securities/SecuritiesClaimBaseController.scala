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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import play.api.libs.json.Format
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

trait SecuritiesClaimBaseController extends ClaimBaseController with SecuritiesClaimRouter {

  final type Claim = SecuritiesClaim

  final val format: Format[SecuritiesClaim] =
    SecuritiesClaim.format

  final override val startOfTheClaim: Call =
    uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.StartController.start()

  final override val checkYourAnswers: Call            = routes.CheckYourAnswersController.show
  final override val claimSubmissionConfirmation: Call = routes.CheckYourAnswersController.showConfirmation

  final override def getClaim(sessionData: SessionData): Option[SecuritiesClaim] =
    sessionData.securitiesClaim

  final override def updateClaim(sessionData: SessionData, claim: SecuritiesClaim): SessionData =
    sessionData.copy(securitiesClaim = Some(claim))
}
