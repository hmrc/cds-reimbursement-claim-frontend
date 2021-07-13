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

import cats.data.EitherT
import org.scalamock.handlers.CallHandler3
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.SubmitClaimResult.SubmitClaimError

import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.CompleteClaim.CompleteC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.CompleteDeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DetailsRegisteredWithCdsAnswer.CompleteDetailsRegisteredWithCdsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.{FillingOutClaim, JustSubmittedClaim, SubmitClaimFailed}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, DutiesSelectedAnswer, SupportingEvidenceAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.{SubmitClaimRequest, SubmitClaimResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BasisOfClaimAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimsAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.CommoditiesDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.CompleteClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DeclarantTypeAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DetailsRegisteredWithCdsAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutiesSelectedAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.NorthernIrelandAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SubmissionResponseGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckYourAnswersAndSubmitControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  val mockClaimService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  lazy val controller: CheckYourAnswersAndSubmitController = instanceOf[CheckYourAnswersAndSubmitController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def mockSubmitClaim(submitClaimRequest: SubmitClaimRequest)(
    response: Either[Error, SubmitClaimResponse]
  ): CallHandler3[SubmitClaimRequest, Lang, HeaderCarrier, EitherT[Future, Error, SubmitClaimResponse]] =
    (mockClaimService
      .submitClaim(_: SubmitClaimRequest, _: Lang)(_: HeaderCarrier))
      .expects(submitClaimRequest, *, *)
      .returning(EitherT.fromEither[Future](response))

  private def sessionWithCompleteClaimState(
  ): (SessionData, JustSubmittedClaim, CompleteC285Claim) = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val completeC285Claim   = sample[CompleteC285Claim]
    val submissionResponse  = sample[SubmitClaimResponse]

    val journey = JustSubmittedClaim(ggCredId, signedInUserDetails, completeC285Claim, submissionResponse)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      completeC285Claim
    )
  }

  val mrn: MRN                                                                          = sample[MRN]
  val completeDeclarantTypeAnswer: CompleteDeclarantTypeAnswer                          = sample[CompleteDeclarantTypeAnswer]
  val completeClaimantDetailsAsIndividualAnswer: CompleteDetailsRegisteredWithCdsAnswer =
    sample[CompleteDetailsRegisteredWithCdsAnswer]
  val basisOfClaim: BasisOfClaim                                                        = sample[BasisOfClaim]
  val supportingEvidences: SupportingEvidenceAnswer                                     = sample[SupportingEvidenceAnswer]
  val completeDutiesSelectedAnswer: DutiesSelectedAnswer                                = sample[DutiesSelectedAnswer]
  val commodityDetailsAnswer: CommodityDetails                                          = sample[CommodityDetails]
  val completeNorthernIrelandAnswer: CompleteNorthernIrelandAnswer                      = sample[CompleteNorthernIrelandAnswer]
  val claimsAnswer: ClaimsAnswer                                                        = sample[ClaimsAnswer]

  val filledDraftC285Claim: DraftC285Claim = sample[DraftC285Claim].copy(
    movementReferenceNumber = Some(MovementReferenceNumber(Right(mrn))),
    duplicateMovementReferenceNumberAnswer = None,
    declarationDetailsAnswer = None,
    duplicateDeclarationDetailsAnswer = None,
    declarantTypeAnswer = Some(completeDeclarantTypeAnswer),
    detailsRegisteredWithCdsAnswer = Some(completeClaimantDetailsAsIndividualAnswer),
    contactDetailsAnswer = None,
    bankAccountDetailsAnswer = None,
    basisOfClaimAnswer = Some(basisOfClaim),
    supportingEvidenceAnswer = Some(supportingEvidences),
    dutiesSelectedAnswer = Some(completeDutiesSelectedAnswer),
    commoditiesDetailsAnswer = Some(commodityDetailsAnswer),
    reasonForBasisAndClaimAnswer = None,
    claimNorthernIrelandAnswer = Some(completeNorthernIrelandAnswer.claimNorthernIrelandAnswer),
    displayDeclaration = Some(
      DisplayDeclaration(
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
    ),
    duplicateDisplayDeclaration = None,
    importerEoriNumberAnswer = None,
    declarantEoriNumberAnswer = None,
    claimsAnswer = Some(claimsAnswer)
  )

  val completeC285Claim: CompleteC285Claim = CompleteC285Claim(
    id = filledDraftC285Claim.id,
    movementReferenceNumber = MovementReferenceNumber(Right(mrn)),
    maybeDuplicateMovementReferenceNumberAnswer = None,
    maybeCompleteDeclarationDetailsAnswer = None,
    maybeCompleteDuplicateDeclarationDetailsAnswer = None,
    completeDeclarantTypeAnswer = completeDeclarantTypeAnswer,
    completeDetailsRegisteredWithCdsAnswer = completeClaimantDetailsAsIndividualAnswer,
    maybeContactDetailsAnswer = None,
    maybeBasisOfClaimAnswer = Some(basisOfClaim),
    maybeCompleteBankAccountDetailAnswer = None,
    supportingEvidenceAnswer = supportingEvidences,
    commodityDetailsAnswer = commodityDetailsAnswer,
    completeNorthernIrelandAnswer = Some(completeNorthernIrelandAnswer),
    None,
    maybeDisplayDeclaration = Some(
      DisplayDeclaration(
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
    ),
    maybeDuplicateDisplayDeclaration = None,
    importerEoriNumber = None,
    declarantEoriNumber = None,
    claimsAnswer
  )

  "Check Your Answers And Submit Controller" when {

    "handling requests to check all answers" must {

      def performAction(): Future[Result] = controller.checkAllAnswers()(FakeRequest())

      "redirect to the start of the journey" when {

        "there is no journey status in the session" in {

          val (session, _, _) = sessionWithCompleteClaimState()

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

      "show the confirmation page" in {

        def performAction(): Future[Result] = controller.confirmationOfSubmission()(FakeRequest())

        val (session, _, _) = sessionWithCompleteClaimState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("confirmation-of-submission.title")
        )

      }
    }

    "handling requests to submit a claim" when {

      "the submission is a success" must {

        "show the confirmation page" in {

          def performAction(): Future[Result] = controller.checkAllAnswersSubmit()(FakeRequest())

          val draftClaim = filledDraftC285Claim

          val completelyFilledOutClaim = sample[FillingOutClaim].copy(draftClaim = draftClaim)

          val (session, _, _) = sessionWithCompleteClaimState()

          val submitClaimRequest = SubmitClaimRequest(
            completelyFilledOutClaim.draftClaim.id,
            completeC285Claim,
            completelyFilledOutClaim.signedInUserDetails
          )

          val submitClaimResponse = sample[SubmitClaimResponse]

          val updatedSession = session.copy(journeyStatus = Some(completelyFilledOutClaim))

          val justSubmittedJourney = updatedSession.copy(journeyStatus =
            Some(
              JustSubmittedClaim(
                completelyFilledOutClaim.ggCredId,
                completelyFilledOutClaim.signedInUserDetails,
                completeC285Claim,
                submitClaimResponse
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(submitClaimRequest)(Right(submitClaimResponse))
            mockStoreSession(justSubmittedJourney)(Right(()))
          }

          checkIsRedirect(
            performAction(),
            routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission()
          )

        }

      }

      "the submission is a failure" must {

        "show the submission error page" in {

          def performAction(): Future[Result] = controller.checkAllAnswersSubmit()(FakeRequest())

          val draftClaim = filledDraftC285Claim

          val completelyFilledOutClaim = sample[FillingOutClaim].copy(draftClaim = draftClaim)

          val (session, _, _) = sessionWithCompleteClaimState()

          val submitClaimRequest = SubmitClaimRequest(
            completelyFilledOutClaim.draftClaim.id,
            completeC285Claim,
            completelyFilledOutClaim.signedInUserDetails
          )

          val submitClaimError = sample[SubmitClaimError]

          val updatedSession = session.copy(journeyStatus = Some(completelyFilledOutClaim))

          val submissionFailed = updatedSession.copy(journeyStatus =
            Some(
              SubmitClaimFailed(
                completelyFilledOutClaim.ggCredId,
                completelyFilledOutClaim.signedInUserDetails
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(submitClaimRequest)(Left(submitClaimError.error))
            mockStoreSession(submissionFailed)(Right(()))
          }

          checkIsRedirect(
            performAction(),
            routes.CheckYourAnswersAndSubmitController.submissionError()
          )

        }

      }

    }

    "handling requests with a submission error session" must {

      def performAction(): Future[Result] = controller.submissionError()(FakeRequest())

      "redirect to the start of the journey" when {

        "the journey is other than a failed submission" in {

          val draftClaim = filledDraftC285Claim

          val completelyFilledOutClaim = sample[FillingOutClaim].copy(draftClaim = draftClaim)

          val (session, _, _) = sessionWithCompleteClaimState()

          val updatedSession = session.copy(journeyStatus = Some(completelyFilledOutClaim))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(
            performAction(),
            baseRoutes.StartController.start()
          )

        }

      }

      "redirect to the confirmation page" when {

        "the claim has just been submitted" in {

          val draftClaim = filledDraftC285Claim

          val completelyFilledOutClaim = sample[FillingOutClaim].copy(draftClaim = draftClaim)

          val (session, _, _) = sessionWithCompleteClaimState()

          val submitClaimResponse = sample[SubmitClaimResponse]

          val updatedSession = session.copy(journeyStatus = Some(completelyFilledOutClaim))

          val justSubmittedJourney = updatedSession.copy(journeyStatus =
            Some(
              JustSubmittedClaim(
                completelyFilledOutClaim.ggCredId,
                completelyFilledOutClaim.signedInUserDetails,
                completeC285Claim,
                submitClaimResponse
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(justSubmittedJourney)
          }

          checkIsRedirect(
            performAction(),
            routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission()
          )

        }

      }

    }

    "show a technical error page" when {

      "the user has not completely filled in the claim" in {

        def performAction(): Future[Result] = controller.checkAllAnswers()(FakeRequest())

        val draftC285Claim = sample[DraftC285Claim].copy(commoditiesDetailsAnswer = None)

        val fillingOutClaim = sample[FillingOutClaim].copy(draftClaim = draftC285Claim)

        val (session, _, _) = sessionWithCompleteClaimState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkIsTechnicalErrorPage(performAction())

      }

      "the submission was a success but the session could not be updated" in {

        def performAction(): Future[Result] = controller.checkAllAnswersSubmit()(FakeRequest())

        val draftClaim = filledDraftC285Claim

        val completelyFilledOutClaim = sample[FillingOutClaim].copy(draftClaim = draftClaim)

        val (session, _, _) = sessionWithCompleteClaimState()

        val submitClaimRequest = SubmitClaimRequest(
          completelyFilledOutClaim.draftClaim.id,
          completeC285Claim,
          completelyFilledOutClaim.signedInUserDetails
        )

        val submitClaimResponse = sample[SubmitClaimResponse]

        val updatedSession = session.copy(journeyStatus = Some(completelyFilledOutClaim))

        val justSubmittedJourney = updatedSession.copy(journeyStatus =
          Some(
            JustSubmittedClaim(
              completelyFilledOutClaim.ggCredId,
              completelyFilledOutClaim.signedInUserDetails,
              completeC285Claim,
              submitClaimResponse
            )
          )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
          mockSubmitClaim(submitClaimRequest)(Right(submitClaimResponse))
          mockStoreSession(justSubmittedJourney)(Left(Error("BOOM!")))
        }

        checkIsTechnicalErrorPage(performAction())

      }

    }

    "SubmissionError" should {
      "render the error page when the submission fails" in {
        def performAction() = controller.submissionError()(FakeRequest())

        val ggCredId            = sample[GGCredId]
        val email               = sample[Email]
        val eori                = sample[Eori]
        val signedInUserDetails =
          SignedInUserDetails(Some(email), eori, Email("email@email.com"), ContactName("Fred Bread"))

        val journeyStatus = SubmitClaimFailed(ggCredId, signedInUserDetails)
        val session       = SessionData.empty.copy(journeyStatus = Some(journeyStatus))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val page = performAction()
        checkPageIsDisplayed(page, messages("submit-claim-error.title"))

      }
    }
  }

}
