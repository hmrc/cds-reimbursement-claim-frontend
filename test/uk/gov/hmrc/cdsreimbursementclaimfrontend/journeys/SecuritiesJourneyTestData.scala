/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._

trait SecuritiesJourneyTestData extends JourneyTestData {

  val emptyJourney: SecuritiesJourney =
    SecuritiesJourney.empty(exampleEori)

  def tryBuildSecuritiesJourney(
    userEoriNumber: Eori,
    mrn: MRN,
    reasonForSecurity: ReasonForSecurity,
    displayDeclaration: DisplayDeclaration,
    similarClaimExistAlreadyInCDFPay: Boolean,
    selectedSecurityDepositIds: Seq[String],
    exportMrnAndDeclaration: Option[(MRN, DEC91Response)]
  ): Either[String, SecuritiesJourney] =
    SecuritiesJourney
      .empty(userEoriNumber)
      .submitMovementReferenceNumber(mrn)
      .submitReasonForSecurityAndDeclaration(reasonForSecurity, displayDeclaration)
      .flatMap(_.submitClaimDuplicateCheckStatus(similarClaimExistAlreadyInCDFPay))
      .tryWhenDefined(exportMrnAndDeclaration)(journey => { case (exportMrn, dec91) =>
        journey.submitExportMovementReferenceNumberAndDeclaration(exportMrn, dec91)
      })
      .flatMap(_.selectSecurityDepositIds(selectedSecurityDepositIds))

}
