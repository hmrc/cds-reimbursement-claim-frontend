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

import cats.Functor
import cats.Id
import org.jsoup.Jsoup
import org.scalatest.prop.TableDrivenPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, SelectNumberOfClaimsAnswer, SessionData, SignedInUserDetails}
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.checkDeclarationDetailsKey

import scala.concurrent.Future

class CheckDeclarationDetailsControllerSpec
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
    JourneyBindable.Bulk,
    JourneyBindable.Scheduled
  )

  lazy val controller: CheckDeclarationDetailsController = instanceOf[CheckDeclarationDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeDisplayDeclaration: Option[DisplayDeclaration],
    numberOfClaims: Option[SelectNumberOfClaimsAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(
        displayDeclaration = maybeDisplayDeclaration,
        movementReferenceNumber = sampleMrnAnswer(),
        selectNumberOfClaimsAnswer = numberOfClaims
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

      "there is no journey status in the session" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.show(journey)(FakeRequest())

        val (session, _, _) = sessionWithClaimState(None, Some(toSelectNumberOfClaims(journey)))

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

    def getAcc14Response(): DisplayDeclaration = {
      val ndrcDetails = sample[NdrcDetails]
      Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(List(ndrcDetails))))
      )
    }

    "display the page" when {

      "there is a declaration" in forAll(journeys) { journey =>
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

        val draftC285Claim                = sessionWithClaimState(Some(displayDeclaration), Some(toSelectNumberOfClaims(journey)))._3
        val (session, fillingOutClaim, _) =
          sessionWithClaimState(Some(displayDeclaration), Some(toSelectNumberOfClaims(journey)))

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

    }

    "redirect user" when {

      "there is no declaration" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.show(journey)(FakeRequest())

        val draftC285Claim                = sessionWithClaimState(None, Some(toSelectNumberOfClaims(journey)))._3
        val (session, fillingOutClaim, _) = sessionWithClaimState(None, Some(toSelectNumberOfClaims(journey)))

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

      "the user confirms the details are correct" in {
        val session =
          sessionWithClaimState(Some(getAcc14Response()), Some(toSelectNumberOfClaims(JourneyBindable.Single)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session._1)
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(JourneyBindable.Single, Seq(checkDeclarationDetailsKey -> "0")),
          routes.SelectWhoIsMakingTheClaimController.selectDeclarantType(JourneyBindable.Single)
        )
      }

      "the user confirms the details are incorrect" in forAll(journeys) { journey =>
        val session = sessionWithClaimState(Some(getAcc14Response()), Some(toSelectNumberOfClaims(journey)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session._1)
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(journey, Seq(checkDeclarationDetailsKey -> "1")),
          routes.EnterMovementReferenceNumberController.enterJourneyMrn(journey)
        )
      }

      "the user sumbits no answer" in forAll(journeys) { journey =>
        val session = sessionWithClaimState(Some(getAcc14Response()), Some(toSelectNumberOfClaims(journey)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session._1)
        }

        checkPageIsDisplayed(
          performAction(journey, Seq.empty),
          messageFromMessageKey("check-declaration-details.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              s"$checkDeclarationDetailsKey.error.required"
            ),
          BAD_REQUEST
        )
      }

      "the user submits an incorrect answer" in forAll(journeys) { journey =>
        forAll(Table("incorrect answers", "2", "")) { incorrectAnswer =>
          val session = sessionWithClaimState(Some(getAcc14Response()), Some(toSelectNumberOfClaims(journey)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session._1)
          }

          checkPageIsDisplayed(
            performAction(journey, Seq(checkDeclarationDetailsKey -> incorrectAnswer)),
            messageFromMessageKey("check-declaration-details.title"),
            doc =>
              doc
                .select(".govuk-error-summary__list > li > a")
                .text() shouldBe messageFromMessageKey(
                s"$checkDeclarationDetailsKey.error.invalid"
              ),
            BAD_REQUEST
          )
        }
      }

      "the user submits a valid answer, but mongodb is down" in forAll(journeys) { journey =>
        val session = sessionWithClaimState(Some(getAcc14Response()), Some(toSelectNumberOfClaims(journey)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session._1)
          mockStoreSession(Left(Error("Mongo is Down")))
        }

        checkPageIsDisplayed(
          performAction(journey, Seq(checkDeclarationDetailsKey -> "0")),
          "Sorry, we’re experiencing technical difficulties",
          _ => (),
          INTERNAL_SERVER_ERROR
        )
      }

    }

  }
}
