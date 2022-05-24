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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.{routes => overpaymentsScheduledRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.MrnImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.ThirdPartyImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes

class RouterSpec extends AnyWordSpec with Matchers with TableDrivenPropertyChecks {

  val allRoutes = Table(
    "Routes",
    MRNSingleRoutes,
    MRNMultipleRoutes,
    MRNScheduledRoutes
  )

  "The next page after the enter movement reference number" must {

    "check declaration details when the user is the importer" in {
      forAll(Table("EntryRoutes", MRNSingleRoutes, MRNMultipleRoutes, MRNMultipleRoutes)) { router =>
        router.nextPageForEnterMRN(MrnImporter(sample[DisplayDeclaration])) shouldBe
          claimRoutes.CheckDeclarationDetailsController.show(router.journeyBindable)
      }
    }

    "enter importer Eori number when there is a 3rd party importer" in {
      forAll(Table("EntryRoutes", MRNSingleRoutes, MRNMultipleRoutes, MRNMultipleRoutes)) { router =>
        router.nextPageForEnterMRN(ThirdPartyImporter(sample[DisplayDeclaration])) shouldBe
          OverpaymentsRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber(router.journeyBindable)
      }
    }
  }

  "The next page after check declaration details" must {

    val scheduledRoutes = Table("Scheduled routes", MRNScheduledRoutes)
    val singleRoutes    = Table("Single routes", MRNSingleRoutes)

    "be upload schedule for the schedule journey" in forAll(scheduledRoutes) { router =>
      router.nextPageForCheckDeclarationDetails(
        whetherDeclarationDetailsCorrect = Yes,
        hasAssociatedMrns = false
      ) should be(
        overpaymentsScheduledRoutes.UploadMrnListController.show
      )
    }

    "be select who is making claim for the single journey" in forAll(singleRoutes) { router =>
      router.nextPageForCheckDeclarationDetails(
        whetherDeclarationDetailsCorrect = Yes,
        hasAssociatedMrns = false
      ) should be(
        claimRoutes.CheckContactDetailsMrnController.show(router.journeyBindable)
      )
    }

    "be enter journey MRN having incorrect declaration" in forAll(allRoutes) { router =>
      router.nextPageForCheckDeclarationDetails(
        whetherDeclarationDetailsCorrect = No,
        hasAssociatedMrns = false
      ) should be(
        OverpaymentsRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(router.journeyBindable)
      )
    }
  }

  "Submit urls in templates must point to the same controller" when {

    "CheckDeclarationDetails" in {
      forAll(allRoutes) { router =>
        router.submitUrlForCheckDeclarationDetails() shouldBe claimRoutes.CheckDeclarationDetailsController.submit(
          router.journeyBindable
        )
      }
    }

    "CheckDuplicateDeclarationDetails" in {
      forAll(allRoutes) { router =>
        router.submitUrlForCheckDuplicateDeclarationDetails() shouldBe claimRoutes.CheckDuplicateDeclarationDetailsController
          .submit(router.journeyBindable)
      }
    }

  }
}
