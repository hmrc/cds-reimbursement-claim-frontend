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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import org.jsoup.Jsoup
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SessionData, SignedInUserDetails}
import play.api.test.Helpers._

import scala.concurrent.Future

class CheckDeclarationDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: CheckDeclarationDetailsController = instanceOf[CheckDeclarationDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeDisplayDeclaration: Option[DisplayDeclaration]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(
        displayDeclaration = maybeDisplayDeclaration,
        movementReferenceNumber = sampleMrnAnswer()
      )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftC285Claim
    )
  }

  //TODO: refactor
  private def sessionWithClaimStateForDuplicate(
    maybeDisplayDeclaration: Option[DisplayDeclaration]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(
        duplicateDisplayDeclaration = maybeDisplayDeclaration,
        movementReferenceNumber = sampleMrnAnswer()
      )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftC285Claim
    )
  }

  "Check Declaration Details Controller" must {

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in {

        def performAction(): Future[Result] = controller.checkDetails()(FakeRequest())

        val (session, _, _) = sessionWithClaimState(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.StartController.start()
        )

      }

      "there is no journey status in the session for duplicate declaration details" in {

        def performAction(): Future[Result] = controller.checkDuplicateDetails()(FakeRequest())

        val (session, _, _) = sessionWithClaimStateForDuplicate(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.StartController.start()
        )

      }

    }

    "display the page" when {

      "there is a declaration" in {
        def performAction(): Future[Result] = controller.checkDetails()(FakeRequest())

        val displayDeclaration = DisplayDeclaration(
          displayResponseDetail = DisplayResponseDetail(
            declarantReferenceNumber = Some("declarant ref"),
            securityReason = Some("security reason"),
            btaDueDate = None,
            btaSource = None,
            declarationId = "declaration-id",
            acceptanceDate = "2020-10-20",
            procedureCode = "p-1",
            consigneeDetails = Some(
              ConsigneeDetails(
                consigneeEORI = "ConsigneeEori",
                legalName = "Gorwand Ukram",
                establishmentAddress = EstablishmentAddress(
                  addressLine1 = "consigne-line-1",
                  addressLine2 = None,
                  addressLine3 = None,
                  postalCode = None,
                  countryCode = "GB"
                ),
                contactDetails = None
              )
            ),
            accountDetails = None,
            bankDetails = None,
            maskedBankDetails = None,
            ndrcDetails = Some(
              List(
                NdrcDetails(
                  taxType = "A01",
                  amount = "20.00",
                  paymentMethod = "CC",
                  paymentReference = "Some ref",
                  cmaEligible = None
                )
              )
            ),
            declarantDetails = DeclarantDetails(
              declarantEORI = "F-1",
              legalName = "Fred Bread",
              establishmentAddress = EstablishmentAddress(
                addressLine1 = "declarant-line-1",
                addressLine2 = None,
                addressLine3 = None,
                postalCode = None,
                countryCode = "GB"
              ),
              contactDetails = None
            )
          )
        )

        val draftC285Claim                = sessionWithClaimState(Some(displayDeclaration))._3
        val (session, fillingOutClaim, _) = sessionWithClaimState(Some(displayDeclaration))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        val action = performAction()

        checkPageIsDisplayed(
          action,
          messageFromMessageKey("check-declaration-details.title")
        )

        val content     = Jsoup.parse(contentAsString(action))
        val tableValues = content.getElementsByClass("govuk-summary-list__value")
        tableValues.get(4).text() shouldBe "consigne-line-1, GB"
        tableValues.get(5).text() shouldBe "declarant-line-1, GB"
      }

      "there is a duplicate declaration" in {
        def performAction(): Future[Result] = controller.checkDuplicateDetails()(FakeRequest())

        val displayDeclaration = DisplayDeclaration(
          displayResponseDetail = DisplayResponseDetail(
            declarantReferenceNumber = Some("declarant ref"),
            securityReason = Some("security reason"),
            btaDueDate = None,
            btaSource = None,
            declarationId = "declaration-id",
            acceptanceDate = "2020-10-20",
            procedureCode = "p-1",
            consigneeDetails = None,
            accountDetails = None,
            bankDetails = None,
            maskedBankDetails = None,
            ndrcDetails = Some(
              List(
                NdrcDetails(
                  taxType = "A01",
                  amount = "20.00",
                  paymentMethod = "CC",
                  paymentReference = "Some ref",
                  cmaEligible = None
                )
              )
            ),
            declarantDetails = DeclarantDetails(
              declarantEORI = "F-1",
              legalName = "Fred Bread",
              establishmentAddress = EstablishmentAddress(
                addressLine1 = "line-1",
                addressLine2 = None,
                addressLine3 = None,
                postalCode = None,
                countryCode = "GB"
              ),
              contactDetails = None
            )
          )
        )

        val draftC285Claim                = sessionWithClaimStateForDuplicate(Some(displayDeclaration))._3
        val (session, fillingOutClaim, _) = sessionWithClaimStateForDuplicate(Some(displayDeclaration))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("check-declaration-details.title")
        )
      }

    }

    "redirect user" when {

      "there is no declaration" in {
        def performAction(): Future[Result] = controller.checkDetails()(FakeRequest())

        val draftC285Claim                = sessionWithClaimState(None)._3
        val (session, fillingOutClaim, _) = sessionWithClaimState(None)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds()
        )
      }

      "there is no duplicate declaration" in {
        def performAction(): Future[Result] = controller.checkDuplicateDetails()(FakeRequest())

        val draftC285Claim                = sessionWithClaimStateForDuplicate(None)._3
        val (session, fillingOutClaim, _) = sessionWithClaimStateForDuplicate(None)

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds()
        )
      }

    }

    "handle submit requests" when {

      "the user confirms the details are correct" in {

        def performAction(): Future[Result] = controller.checkDetailsSubmit()(FakeRequest())
        val displayDeclaration              = sample[DisplayDeclaration]

        val session = sessionWithClaimState(Some(displayDeclaration))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session._1)
        }

        checkIsRedirect(
          performAction(),
          routes.SelectWhoIsMakingTheClaimController.selectDeclarantType()
        )
      }

      "the user confirms the duplicate details are correct" in {

        def performAction(): Future[Result] = controller.checkDuplicateDetailsSubmit()(FakeRequest())
        val displayDeclaration              = sample[DisplayDeclaration]

        val session = sessionWithClaimStateForDuplicate(Some(displayDeclaration))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session._1)
        }

        checkIsRedirect(
          performAction(),
          routes.EnterCommoditiesDetailsController.enterCommoditiesDetails()
        )
      }

    }

  }

}
