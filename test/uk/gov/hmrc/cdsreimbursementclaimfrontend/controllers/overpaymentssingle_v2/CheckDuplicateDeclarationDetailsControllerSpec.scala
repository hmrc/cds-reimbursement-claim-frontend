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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2
import org.jsoup.Jsoup
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status.BAD_REQUEST
import play.api.http.Status.NOT_FOUND
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class CheckDuplicateDeclarationDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val mockClaimService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  val controller: CheckDuplicateDeclarationDetailsController = instanceOf[CheckDuplicateDeclarationDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Overpayments_v2)

  val messagesKey: String = "check-declaration-details"

  val journeyGen: Gen[OverpaymentsSingleJourney] =
    for {
      j1  <- buildJourneyGen(answersUpToBasisForClaimGen())
               .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
      mrn <- genMRN
      decl = buildDisplayDeclaration(
               id = mrn.value,
               declarantEORI = j1.getDeclarantEoriFromACC14.getOrElse(exampleEori),
               consigneeEORI = j1.getConsigneeEoriFromACC14
             )
    } yield j1
      .submitDuplicateMovementReferenceNumberAndDeclaration(mrn, decl)
      .getOrFail

  "Check Duplicate Declaration Details Controller" when {
    "Check Duplicate Declaration Details page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "does not find the page if the rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if duplicate declaration exists" in {
        val journey =
          journeyGen.sample.getOrElse(fail("Journey building has failed."))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            val expectedMainParagraph = Jsoup.parse(messageFromMessageKey(s"$messagesKey.help-text")).text()

            doc
              .select("main p")
              .get(0)
              .text()                                    shouldBe expectedMainParagraph
            doc.select(s"#$messagesKey").attr("checked") shouldBe ""
          }
        )
      }

      "redirect if duplicate declaration not expected" in {
        val journey =
          buildJourneyGen(answersUpToBasisForClaimGen())
            .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DutySuspension))
            .sample
            .get

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterAdditionalDetailsController.show
        )
      }

      "redirect if duplicate declaration expected but not provided" in {
        val journey =
          buildJourneyGen(answersUpToBasisForClaimGen())
            .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
            .sample
            .get

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterDuplicateMovementReferenceNumberController.show
        )
      }
    }

    "Submit Check Duplicate Declaration Details page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Yes/No answer" in {
        val journey: OverpaymentsSingleJourney =
          journeyGen.sample.get

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction("check-declaration-details" -> ""),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            getErrorSummary(doc)                         shouldBe messageFromMessageKey(s"$messagesKey.error.required")
            doc.select(s"#$messagesKey").attr("checked") shouldBe ""
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit when user selects Yes" in {
        val journey: OverpaymentsSingleJourney =
          journeyGen.sample.get

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction("check-declaration-details" -> "true"),
          routes.EnterAdditionalDetailsController.show
        )
      }

      "submit when user selects No" in {
        val journey: OverpaymentsSingleJourney =
          journeyGen.sample.get

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction("check-declaration-details" -> "false"),
          routes.EnterDuplicateMovementReferenceNumberController.submit
        )

      }

    }
  }

}
