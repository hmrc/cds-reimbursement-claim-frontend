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

import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyBindable, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, SessionData, SignedInUserDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.checkDeclarationDetailsKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer

import scala.concurrent.Future

class CheckDuplicateDeclarationDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with TableDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Multiple,
    JourneyBindable.Scheduled
  )

  lazy val controller: CheckDuplicateDeclarationDetailsController =
    instanceOf[CheckDuplicateDeclarationDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimStateForDuplicate(
    maybeDisplayDeclaration: Option[DisplayDeclaration],
    maybeTypeOfClaim: Option[TypeOfClaimAnswer]
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      =
      DraftClaim.blank.copy(
        duplicateDisplayDeclaration = maybeDisplayDeclaration,
        movementReferenceNumber = Some(sample[MRN]),
        typeOfClaim = maybeTypeOfClaim
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

      "there is no journey status in the session for duplicate declaration details" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.show(journey)(FakeRequest())

        val (session, _, _) = sessionWithClaimStateForDuplicate(None, Some(toTypeOfClaim(journey)))

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
      "there is a duplicate declaration" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.show(journey)(FakeRequest())

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

        val draftC285Claim                =
          sessionWithClaimStateForDuplicate(Some(displayDeclaration), Some(toTypeOfClaim(journey)))._3
        val (session, fillingOutClaim, _) =
          sessionWithClaimStateForDuplicate(Some(displayDeclaration), Some(toTypeOfClaim(journey)))

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

      "there is no duplicate declaration" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.show(journey)(FakeRequest())

        val draftC285Claim                = sessionWithClaimStateForDuplicate(None, Some(toTypeOfClaim(journey)))._3
        val (session, fillingOutClaim, _) =
          sessionWithClaimStateForDuplicate(None, Some(toTypeOfClaim(journey)))

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

      def performAction(journey: JourneyBindable, data: Seq[(String, String)]): Future[Result] =
        controller.submit(journey)(FakeRequest().withFormUrlEncodedBody(data: _*))

      //TODO Update Redirect url when decision is made
      "the user confirms the duplicate details are correct" in forAll(journeys) { journey =>
        val displayDeclaration = sample[DisplayDeclaration]
        val session            = sessionWithClaimStateForDuplicate(Some(displayDeclaration), Some(toTypeOfClaim(journey)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session._1)
        }

        checkIsRedirect(
          performAction(journey, Seq(checkDeclarationDetailsKey -> "true")),
          routes.EnterCommoditiesDetailsController.enterCommoditiesDetails(journey)
        )
      }

      //TODO Update Redirect url when decision is made
      "the user confirms the details are incorrect" in forAll(journeys) { journey =>
        val displayDeclaration = sample[DisplayDeclaration]
        val session            = sessionWithClaimStateForDuplicate(Some(displayDeclaration), Some(toTypeOfClaim(journey)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session._1)
        }

        checkPageIsDisplayed(
          performAction(journey, Seq(checkDeclarationDetailsKey -> "false")),
          "Tell us your duplicate Movement Reference Number (MRN)"
        )
      }

      //TODO Write testcase for: User submits no answer

      //TODO Write testcase for: User submits an incorrect answer

    }

  }
}
