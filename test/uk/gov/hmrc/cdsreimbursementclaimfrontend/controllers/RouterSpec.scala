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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.{DeclarationAnswersAreCorrect, DeclarationAnswersAreIncorrect}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{JourneyBindable, routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.schedule.{routes => scheduleRoutes}
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
          claimRoutes.CheckDeclarationDetailsController.show(router.journeyBindable)
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
    val nonDuplicateClaims     = BasisOfClaim.allClaimsTypes.filterNot(_ == BasisOfClaim.DuplicateEntry)
    val nonDuplicateClaimTable = Table("Claim", nonDuplicateClaims: _*)

    "enter duplicate reference number when basis of claim has duplicate entry selected" in {
      forAll(allRoutes) { router =>
        router.nextPageForBasisForClaim(
          BasisOfClaim.DuplicateEntry,
          isAmend = false
        ) shouldBe claimRoutes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrn(router.journeyBindable)
      }
    }

    "enter commodities details when basis of claim doesn't have duplicate entry selected" in {
      forAll(allRoutes) { router =>
        forAll(nonDuplicateClaimTable) { basisForClaim =>
          router.nextPageForBasisForClaim(
            basisForClaim,
            isAmend = false
          ) shouldBe claimRoutes.EnterCommoditiesDetailsController
            .enterCommoditiesDetails(router.journeyBindable)
        }
      }
    }

    "redirect to check your answers when answer is amended" in {
      forAll(allRoutes) { router =>
        forAll(nonDuplicateClaimTable) { basisForClaim =>
          router.nextPageForBasisForClaim(
            basisForClaim,
            isAmend = true
          ) shouldBe claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers()
        }
      }
    }
  }

  "The next page after check declaration details" must {

    val scheduledRoutes = Table("Scheduled routes", MRNScheduledRoutes, EntryScheduledRoutes)
    val singleRoutes    = Table("Single routes", MRNSingleRoutes, EntrySingleRoutes)

    "be upload schedule for the schedule journey" in forAll(scheduledRoutes) { router =>
      router.nextPageForCheckDeclarationDetails(DeclarationAnswersAreCorrect) should be(
        scheduleRoutes.ScheduledDocumentController.uploadScheduledDocument()
      )
    }

    "be select who is making claim for the single journey" in forAll(singleRoutes) { router =>
      router.nextPageForCheckDeclarationDetails(DeclarationAnswersAreCorrect) should be(
        claimRoutes.SelectWhoIsMakingTheClaimController.selectDeclarantType(router.journeyBindable)
      )
    }

    "be enter journey MRN having incorrect declaration" in forAll(allRoutes) { router =>
      router.nextPageForCheckDeclarationDetails(DeclarationAnswersAreIncorrect) should be(
        claimRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Scheduled)
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

    "BasisOfClaim" in {
      forAll(allRoutes) { router =>
        router.submitUrlForBasisOfClaim(true) shouldBe claimRoutes.SelectBasisForClaimController
          .changeBasisForClaimSubmit(router.journeyBindable)
      }
    }

    "CommoditiesDetails" in {
      forAll(allRoutes) { router =>
        router.submitUrlForCommoditiesDetails(true) shouldBe claimRoutes.EnterCommoditiesDetailsController
          .changeCommoditiesDetailsSubmit(router.journeyBindable)
      }
    }

    "WhoIsMakingTheClaim" in {
      forAll(allRoutes) { router =>
        router.submitUrlForWhoIsMakingTheClaim(true) shouldBe claimRoutes.SelectWhoIsMakingTheClaimController
          .changeDeclarantTypeSubmit(router.journeyBindable)
      }
    }

    "ClaimNorthernIreland" in {
      forAll(allRoutes) { router =>
        router.submitUrlForClaimNorthernIreland(true) shouldBe claimRoutes.ClaimNorthernIrelandController
          .changeNorthernIrelandClaimSubmit(router.journeyBindable)
      }
    }

  }

}
