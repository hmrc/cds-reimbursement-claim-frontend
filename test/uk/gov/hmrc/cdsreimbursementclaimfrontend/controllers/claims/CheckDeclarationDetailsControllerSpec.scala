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

import cats.{Functor, Id}
import org.jsoup.Jsoup
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.checkDeclarationDetailsKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyBindable, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BigDecimalOps, DraftClaim, SessionData, SignedInUserDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.HtmlParseSupport

import scala.collection.JavaConverters._
import scala.concurrent.Future

class CheckDeclarationDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with HtmlParseSupport
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

  lazy val controller: CheckDeclarationDetailsController = instanceOf[CheckDeclarationDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeDisplayDeclaration: Option[DisplayDeclaration],
    maybeTypeOfClaim: Option[TypeOfClaimAnswer]
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      =
      DraftClaim.blank.copy(
        displayDeclaration = maybeDisplayDeclaration,
        movementReferenceNumber = Some(sample[MRN]),
        typeOfClaim = maybeTypeOfClaim
      )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copyWith(
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

        val (session, _, _) = sessionWithClaimState(None, Some(toTypeOfClaim(journey)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copyWith(journeyStatus = None))
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

        val displayDeclaration = sample[DisplayDeclaration]

        val draftC285Claim                = sessionWithClaimState(Some(displayDeclaration), Some(toTypeOfClaim(journey)))._3
        val (session, fillingOutClaim, _) =
          sessionWithClaimState(Some(displayDeclaration), Some(toTypeOfClaim(journey)))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copyWith(journeyStatus = Some(updatedJourney)))
        }

        val action = performAction()

        checkPageIsDisplayed(
          action,
          messageFromMessageKey(s"check-declaration-details.title")
        )

        val body        = Jsoup.parse(contentAsString(action))
        val tableValues = body
          .getElementsByClass("govuk-summary-list__value")
          .asScala
          .map(_.text())

        tableValues should contain allElementsOf (
          Seq(
            displayDeclaration.displayResponseDetail.declarationId,
            displayDeclaration.displayResponseDetail.acceptanceDate,
            displayDeclaration.totalPaidCharges.toPoundSterlingString,
            displayDeclaration.declarantName
          ) ++ Seq(
            displayDeclaration.consigneeName,
            displayDeclaration.consigneeEmail,
            displayDeclaration.consigneeTelephone,
            displayDeclaration.consigneeAddress.map(_.replace("<br />", " ")),
            displayDeclaration.declarantContactAddress.map(_.replace("<br />", " "))
          ).flatMap(_.toList)
        )
      }
    }

    "redirect user" when {

      "there is no declaration" in forAll(journeys) { journey =>
        def performAction(): Future[Result] = controller.show(journey)(FakeRequest())

        val draftC285Claim                = sessionWithClaimState(None, Some(toTypeOfClaim(journey)))._3
        val (session, fillingOutClaim, _) = sessionWithClaimState(None, Some(toTypeOfClaim(journey)))

        val updatedJourney = fillingOutClaim.copy(draftClaim = draftC285Claim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copyWith(journeyStatus = Some(updatedJourney)))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.IneligibleController.ineligible()
        )
      }

    }

    "handle submit requests" when {

      def performAction(journey: JourneyBindable, data: Seq[(String, String)]): Future[Result] =
        controller.submit(journey)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "the user confirms the details are correct" in {
        val session =
          sessionWithClaimState(Some(getAcc14Response()), Some(toTypeOfClaim(JourneyBindable.Single)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session._1)
        }

        checkIsRedirect(
          performAction(JourneyBindable.Single, Seq(checkDeclarationDetailsKey -> "true")),
          routes.SelectWhoIsMakingTheClaimController.selectDeclarantType(JourneyBindable.Single)
        )
      }

      "the user confirms the details are incorrect" in forAll(journeys) { journey =>
        val session = sessionWithClaimState(Some(getAcc14Response()), Some(toTypeOfClaim(journey)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session._1)
        }

        checkIsRedirect(
          performAction(journey, Seq(checkDeclarationDetailsKey -> "false")),
          routes.EnterMovementReferenceNumberController.enterJourneyMrn(journey)
        )
      }

      "the user sumbits no answer" in forAll(journeys) { journey =>
        val session = sessionWithClaimState(Some(getAcc14Response()), Some(toTypeOfClaim(journey)))

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
        val session = sessionWithClaimState(Some(getAcc14Response()), Some(toTypeOfClaim(journey)))

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
              s"$checkDeclarationDetailsKey.error.invalid"
            ),
          BAD_REQUEST
        )
      }
    }
  }
}
