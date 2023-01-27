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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple_v2

import cats.implicits.catsSyntaxEq
import org.jsoup.nodes.Element
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AdjustDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genMRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import scala.concurrent.Future
import scala.collection.JavaConverters._

class CheckMovementReferenceNumbersControllerSpec
    extends ControllerSpec
    with AdjustDisplayDeclaration
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with JourneyTestData {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckMovementReferenceNumbersController = instanceOf[CheckMovementReferenceNumbersController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  val formKey: String = "check-movement-reference-numbers"

  def areMrnsUnique(acc14Declarations: List[DisplayDeclaration]): Boolean =
    acc14Declarations.map(_.getMRN).toSet.size == acc14Declarations.size

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  private val session = SessionData.empty.copy(
    overpaymentsMultipleJourney = Some(OverpaymentsMultipleJourney.empty(exampleEori))
  )

  def addAcc14(
    journey: OverpaymentsMultipleJourney,
    acc14Declaration: DisplayDeclaration
  ): Either[String, OverpaymentsMultipleJourney] = {
    val nextIndex           = journey.getMovementReferenceNumbers.map(_.size).getOrElse(0)
    val adjustedDeclaration = adjustWithDeclarantEori(acc14Declaration, journey)
    journey
      .submitMovementReferenceNumberAndDeclaration(nextIndex, adjustedDeclaration.getMRN, adjustedDeclaration)
  }

  "Check Movement Reference Numbers Controller" when {
    "Show Check Movement Reference Numbers page" must {

      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      def validateMrnLine(div: Element, index: Int, possibleMrns: List[MRN], hasDeleteLink: Boolean = true): Boolean = {
        //TODO: Get correct URL
        div.select("dd:nth-of-type(2)").select("a").attr("href") shouldBe routes.EnterMovementReferenceNumberController
          .show(index + 1)
          .url
        if (hasDeleteLink) {
          div.select("dd:nth-of-type(3) a").isEmpty shouldBe false
          val mrn = MRN(div.select("dd:nth-of-type(1)").text())
          possibleMrns should contain(mrn)
          val fullHref = div.select("dd:nth-of-type(3) a").attr("href")
          fullHref shouldBe routes.CheckMovementReferenceNumbersController.delete(mrn).url
        } else {
          div.select("dd:nth-of-type(3) a").isEmpty shouldBe true
        }
        true
      }

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "redirect to enter mrn page if no MRNs contained in journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.EnterMovementReferenceNumberController.show(0)
        )
      }

      "redirect to enter second mrn page if only one MRN in the journey" in forAll { (firstMrn: DisplayDeclaration) =>
        val session =
          SessionData(
            OverpaymentsMultipleJourney
              .empty(exampleEori)
              .submitMovementReferenceNumberAndDeclaration(firstMrn.getMRN, firstMrn)
              .getOrFail
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.EnterMovementReferenceNumberController.show(2)
        )
      }

      "show page with only 2 MRNs" in forAll { (firstMrn: DisplayDeclaration, secondMrn: DisplayDeclaration) =>
        whenever(firstMrn.getMRN =!= secondMrn.getMRN) {
          val journey = (for {
            j1 <- addAcc14(session.overpaymentsMultipleJourney.get, firstMrn)
            j2 <- addAcc14(j1, secondMrn)
          } yield j2).getOrFail
          journey.getMovementReferenceNumbers.get shouldBe Seq(firstMrn.getMRN, secondMrn.getMRN)
          val mrns    = List(firstMrn.getMRN, secondMrn.getMRN)

          val sessionToAmend = session.copy(overpaymentsMultipleJourney = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionToAmend)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$formKey.title"),
            doc => {
              getErrorSummary(doc) shouldBe ""
              formAction(doc)      shouldBe routes.CheckMovementReferenceNumbersController.submit.url
              val lines = doc.select("dl > div").asScala
              lines.size                                                  shouldBe 2
              validateMrnLine(lines.head, 0, mrns, hasDeleteLink = false) shouldBe true
              validateMrnLine(lines.last, 1, mrns, hasDeleteLink = false) shouldBe true
            }
          )
        }
      }

      "show page with more than 2 MRNs" in forAll { acc14Declarations: List[DisplayDeclaration] =>
        whenever(acc14Declarations.size > 2 && areMrnsUnique(acc14Declarations)) {
          val journey = acc14Declarations.foldLeft(session.overpaymentsMultipleJourney.get) {
            case (journey, declaration) => addAcc14(journey, declaration).getOrFail
          }

          journey.getMovementReferenceNumbers.map(_.size) shouldBe Some(acc14Declarations.size)
          val mrns = acc14Declarations.map(_.getMRN)

          val sessionToAmend = session.copy(overpaymentsMultipleJourney = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionToAmend)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$formKey.title"),
            doc => {
              getErrorSummary(doc) shouldBe ""
              formAction(doc)      shouldBe routes.CheckMovementReferenceNumbersController.submit.url
              val lines = doc.select("dl > div").asScala
              lines.size                                                  shouldBe acc14Declarations.size
              validateMrnLine(lines.head, 0, mrns, hasDeleteLink = false) shouldBe true
              lines.zipWithIndex.drop(1).forall(line => validateMrnLine(line._1, line._2, mrns))
            }
          )
        }
      }
    }

    "Submit Check Movement Reference Numbers page" should {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Yes/No answer" in forAll { acc14Declarations: List[DisplayDeclaration] =>
        whenever(acc14Declarations.size > 2 && areMrnsUnique(acc14Declarations)) {
          val journey = acc14Declarations.foldLeft(session.overpaymentsMultipleJourney.get) {
            case (journey, declaration) => addAcc14(journey, declaration).getOrFail
          }

          journey.getMovementReferenceNumbers.map(_.size) shouldBe Some(acc14Declarations.size)

          val sessionToAmend = session.copy(overpaymentsMultipleJourney = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionToAmend)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$formKey.title"),
            doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$formKey.error.invalid"),
            BAD_REQUEST
          )
        }
      }

      "submit when user selects Yes" in forAll { acc14Declarations: List[DisplayDeclaration] =>
        whenever(acc14Declarations.size > 2 && areMrnsUnique(acc14Declarations)) {
          val journey = acc14Declarations.foldLeft(session.overpaymentsMultipleJourney.get) {
            case (journey, declaration) => addAcc14(journey, declaration).getOrFail
          }

          journey.getMovementReferenceNumbers.map(_.size) shouldBe Some(acc14Declarations.size)

          val sessionToAmend = session.copy(overpaymentsMultipleJourney = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionToAmend)
          }

          checkIsRedirect(
            performAction(formKey -> "true"),
            routes.EnterMovementReferenceNumberController.show(acc14Declarations.size + 1)
          )
        }
      }

      "submit when user selects No" in forAll { acc14Declarations: List[DisplayDeclaration] =>
        whenever(acc14Declarations.size > 2 && areMrnsUnique(acc14Declarations)) {
          val journey = acc14Declarations.foldLeft(session.overpaymentsMultipleJourney.get) {
            case (journey, declaration) => addAcc14(journey, declaration).getOrFail
          }

          journey.getMovementReferenceNumbers.map(_.size) shouldBe Some(acc14Declarations.size)

          val sessionToAmend = session.copy(overpaymentsMultipleJourney = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionToAmend)
          }

          checkIsRedirect(
            performAction(formKey -> "false"),
            routes.CheckClaimantDetailsController.show
          )
        }
      }

      "redirect to enter mrn page if no MRNs contained in journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(formKey -> "true"),
          routes.EnterMovementReferenceNumberController.show(0)
        )
      }
    }

    "Delete Movement Reference Numbers" should {

      def performAction(mrn: MRN): Future[Result] =
        controller.delete(mrn)(
          FakeRequest()
        )

      "do not find the page if rejected goods feature is disabled" in forAll(genMRN) { mrn: MRN =>
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction(mrn)) shouldBe NOT_FOUND
      }

      "redirect back to the check movement reference numbers page if remove worked" in forAll {
        acc14Declarations: List[DisplayDeclaration] =>
          whenever(acc14Declarations.size > 3 && areMrnsUnique(acc14Declarations)) {
            val journey = acc14Declarations.foldLeft(session.overpaymentsMultipleJourney.get) {
              case (journey, declaration) => addAcc14(journey, declaration).getOrFail
            }

            journey.getMovementReferenceNumbers.map(_.size) shouldBe Some(acc14Declarations.size)

            val sessionToAmend = session.copy(overpaymentsMultipleJourney = Some(journey))

            val mrn = Gen.oneOf(journey.getMovementReferenceNumbers.get.tail).sample.get

            val updatedJourney = journey.removeMovementReferenceNumberAndDisplayDeclaration(mrn).getOrFail
            val updatedSession = session.copy(overpaymentsMultipleJourney = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionToAmend)
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(mrn),
              routes.CheckMovementReferenceNumbersController.show
            )
          }
      }

      "redirect to ineligible page if anything is wrong with the attempt to remove the MRN" in forAll(genMRN) {
        mrn: MRN =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(mrn),
            baseRoutes.IneligibleController.ineligible()
          )
      }
    }
  }
}
