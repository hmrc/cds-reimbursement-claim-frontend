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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.{ErnImporter, MrnImporter, ThirdPartyImporter}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample

class RouterSpec extends AnyWordSpec with Matchers with TableDrivenPropertyChecks {

  val allRoutes = Table(
    "Routes",
    MRNSingleRoutes,
    MRNBulkRoutes,
    MRNScheduledRoutes,
    EntrySingleRoutes,
    EntryBulkRoutes,
    EntryScheduledRoutes
  )

  "The next page after the enter movement reference number" must {

    "check declaration details when the user is the importer" in {
      forAll(Table("EntryRoutes", MRNSingleRoutes, MRNBulkRoutes, MRNBulkRoutes)) { router =>
        router.nextPageForEnterMRN(MrnImporter(sample[DisplayDeclaration])) shouldBe
          claimRoutes.CheckDeclarationDetailsController.checkDetails()
      }
    }

    "enter importer Eori number when there is a 3rd party importer" in {
      forAll(Table("EntryRoutes", MRNSingleRoutes, MRNBulkRoutes, MRNBulkRoutes)) { router =>
        router.nextPageForEnterMRN(ThirdPartyImporter(sample[DisplayDeclaration])) shouldBe
          claimRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber()
      }
    }

    "enter declaration details when using Entry Number" in {
      forAll(Table("EntryRoutes", EntrySingleRoutes, EntryBulkRoutes, EntryBulkRoutes)) { router =>
        router.nextPageForEnterMRN(ErnImporter) shouldBe
          claimRoutes.EnterDeclarationDetailsController.enterDeclarationDetails()
      }
    }
  }

  "The next page after basis of claim" must {
    val nonDuplicateClaimss    = BasisOfClaim.allClaimsTypes.filterNot(_ == BasisOfClaim.DuplicateEntry)
    val nonDuplicateClaimTable = Table("Claim", nonDuplicateClaimss: _*)

    "enter duplicate reference number when basis of claim has duplicate entry selected" in {
      forAll(allRoutes) { router =>
        router.nextPageForBasisForClaim(
          BasisOfClaim.DuplicateEntry
        ) shouldBe claimRoutes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrn(router.journeyBindable)
      }
    }
    "enter commodities details when basis of claim doesn't have duplicate entry selected" in {
      forAll(allRoutes) { router =>
        forAll(nonDuplicateClaimTable) { basisForClaim =>
          router.nextPageForBasisForClaim(basisForClaim) shouldBe claimRoutes.EnterCommoditiesDetailsController
            .enterCommoditiesDetails()
        }
      }
    }
  }

}
