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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import cats.implicits.catsSyntaxOptionId
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.SelectDutyCodesControllerSpec.genDutyWithRandomlySelectedTaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.dutyTypesOrdering
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.taxCodesOrdering
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

import scala.collection.immutable.SortedMap

class SelectDutyCodesControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: SelectDutyCodesController = instanceOf[SelectDutyCodesController]
  val selectDutyCodesKey: String            = "select-duty-codes"

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2)

  "Select Duty Codes Controller" should {

    "redirect to the select duty types page" when {
      "user has not selected any duty types" in {
        val (session, _) = sessionWithDutyCodesState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.iterate()(FakeRequest()),
          claimsRoutes.SelectDutyTypesController.showDutyTypes(JourneyBindable.Scheduled)
        )
      }
    }

    "show select tax code page" when {
      "user has previously selected duty types" in forAll { duty: DutyType =>
        val (session, _) = sessionWithDutyCodesState(
          SelectedDutyTaxCodesReimbursementAnswer
            .buildFrom(duty :: Nil)
            .synchronizingWith(SelectedDutyTaxCodesReimbursementAnswer.none)
            .some
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.iterate()(FakeRequest()),
          routes.SelectDutyCodesController.showDutyCodes(duty)
        )
      }
    }

    "tick existing tax codes" when {
      "select tax code page is shown" in forAll(genDutyWithRandomlySelectedTaxCode) { case (duty, taxCode) =>
        val (session, _) = sessionWithDutyCodesState(
          SelectedDutyTaxCodesReimbursementAnswer(
            SortedMap(
              duty -> SortedMap(taxCode -> AmountPaidWithCorrect.unclaimed)
            )
          ).some
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.showDutyCodes(duty)(FakeRequest()),
          messageFromMessageKey(
            s"$selectDutyCodesKey.title",
            messageFromMessageKey(s"$selectDutyCodesKey.h1.${duty.repr}")
          ),
          doc => {
            isCheckboxChecked(doc, taxCode.value) shouldBe true
            formAction(doc)                       shouldBe routes.SelectDutyCodesController.submitDutyCodes(duty).url
          }
        )
      }
    }

    "save user selected tax codes and ask user to select tax codes for the next available duty" in {
      forAll(Gen.oneOf(DutyTypes.custom), Gen.oneOf(DutyTypes.excise)) { (customDuty, exciseDuty) =>
        val (session, draftClaim) = sessionWithDutyCodesState(
          SelectedDutyTaxCodesReimbursementAnswer(
            SortedMap(
              customDuty -> SortedMap.empty[TaxCode, AmountPaidWithCorrect],
              exciseDuty -> SortedMap.empty[TaxCode, AmountPaidWithCorrect]
            )
          ).some
        )

        val updatedSession: SessionData =
          session.copy(journeyStatus = session.journeyStatus.collect { case fillingOutClaim: FillingOutClaim =>
            fillingOutClaim.copy(
              draftClaim = draftClaim.copy(
                selectedDutyTaxCodesReimbursementAnswer = SelectedDutyTaxCodesReimbursementAnswer(
                  SortedMap(
                    customDuty -> SortedMap(customDuty.taxCodes(0) -> AmountPaidWithCorrect.unclaimed),
                    exciseDuty -> SortedMap.empty
                  )
                ).some
              )
            )
          })

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          controller.submitDutyCodes(customDuty)(
            FakeRequest().withFormUrlEncodedBody(s"$selectDutyCodesKey[]" -> customDuty.taxCodes(0).value)
          ),
          routes.SelectDutyCodesController.showDutyCodes(exciseDuty)
        )
      }
    }

    "save user selected tax codes and redirect to the next page" when {
      "no other selected duties remaining" in forAll(genDutyWithRandomlySelectedTaxCode) { case (duty, taxCode) =>
        val (session, draftClaim) = sessionWithDutyCodesState(
          SelectedDutyTaxCodesReimbursementAnswer(SortedMap(duty -> SortedMap.empty)).some
        )

        val updatedSession: SessionData =
          session.copy(journeyStatus = session.journeyStatus.collect { case fillingOutClaim: FillingOutClaim =>
            fillingOutClaim.copy(
              draftClaim = draftClaim.copy(
                selectedDutyTaxCodesReimbursementAnswer = SelectedDutyTaxCodesReimbursementAnswer(
                  SortedMap(duty -> SortedMap(taxCode -> AmountPaidWithCorrect.unclaimed))
                ).some
              )
            )
          })

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          controller.submitDutyCodes(duty)(
            FakeRequest().withFormUrlEncodedBody(s"$selectDutyCodesKey[]" -> taxCode.value)
          ),
          routes.EnterScheduledClaimController.iterate()
        )
      }
    }

    "show an error summary" when {
      "no duty code is selected" in forAll { duty: DutyType =>
        val (session, _) = sessionWithDutyCodesState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.submitDutyCodes(duty)(FakeRequest()),
          messageFromMessageKey(
            s"$selectDutyCodesKey.title",
            messageFromMessageKey(s"$selectDutyCodesKey.h1.${duty.repr}")
          ),
          doc =>
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"$selectDutyCodesKey.error.required"
            ),
          BAD_REQUEST
        )
      }
    }
  }

  private def sessionWithDutyCodesState(
    selectedDutyTaxCodesReimbursementAnswer: Option[SelectedDutyTaxCodesReimbursementAnswer] = None
  ): (SessionData, DraftClaim) = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val draftC285Claim      = DraftClaim.blank.copy(
      selectedDutyTaxCodesReimbursementAnswer = selectedDutyTaxCodesReimbursementAnswer
    )
    (
      SessionData.empty.copy(journeyStatus = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim).some),
      draftC285Claim
    )
  }
}

object SelectDutyCodesControllerSpec {

  lazy val genDutyWithRandomlySelectedTaxCode: Gen[(DutyType, TaxCode)] = for {
    duty    <- genDuty
    taxCode <- Gen.oneOf(duty.taxCodes)
  } yield (duty, taxCode)
}
