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
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.SubmitClaimResult.SubmitClaimError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DeclarantTypeAnswer.AssociatedWithRepresentativeCompany
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.{FillingOutClaim, JustSubmittedClaim, SubmitClaimFailed}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{ContactAddress, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{ClaimsAnswer, DutiesSelectedAnswer, SupportingEvidencesAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.{SubmitClaimRequest, SubmitClaimResponse}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BasisOfClaimAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ClaimsAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.CommoditiesDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.CompleteClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutiesSelectedAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{alphaCharGen, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyBindableGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.NorthernIrelandAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SubmissionResponseGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SignedInUserDetails, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckYourAnswersAndSubmitControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

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

  private def sessionWithCompleteClaimState(): (SessionData, JustSubmittedClaim, CompleteClaim) = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val completeC285Claim   = sample[CompleteClaim]
    val submissionResponse  = sample[SubmitClaimResponse]
    val journeyBindable     = sample[JourneyBindable]

    val journey =
      JustSubmittedClaim(ggCredId, signedInUserDetails, completeC285Claim, submissionResponse, journeyBindable)

    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      completeC285Claim
    )
  }

  val mrn: MRN                                                         = sample[MRN]
  val declarantTypeAnswer: DeclarantTypeAnswer                         = AssociatedWithRepresentativeCompany
  val basisOfClaim: BasisOfClaim                                       = sample[BasisOfClaim]
  val supportingEvidences: SupportingEvidencesAnswer                   = sample[SupportingEvidencesAnswer]
  val completeDutiesSelectedAnswer: DutiesSelectedAnswer               = sample[DutiesSelectedAnswer]
  val commodityDetailsAnswer: CommodityDetails                         = sample[CommodityDetails]
  val northernIrelandAnswer: ClaimNorthernIrelandAnswer                = sample[ClaimNorthernIrelandAnswer]
  val claimsAnswer: ClaimsAnswer                                       = sample[ClaimsAnswer]
  val signedInUserDetails: SignedInUserDetails                         = sample[SignedInUserDetails]
  val declarantEstablishmentAddress: EstablishmentAddress              = EstablishmentAddress(
    addressLine1 = alphaCharGen(10),
    addressLine2 = None,
    addressLine3 = None,
    postalCode = Some(alphaCharGen(10)),
    countryCode = "GB"
  )
  val declarantDetails: DeclarantDetails                               = DeclarantDetails(
    declarantEORI = "F-1",
    legalName = "Fred Bread",
    establishmentAddress = declarantEstablishmentAddress,
    contactDetails = None
  )
  val detailsRegisteredWithCdsFormData: DetailsRegisteredWithCdsAnswer = DetailsRegisteredWithCdsAnswer(
    fullName = declarantDetails.legalName,
    emailAddress = signedInUserDetails.verifiedEmail,
    contactAddress = ContactAddress(
      line1 = declarantEstablishmentAddress.addressLine1,
      line2 = declarantEstablishmentAddress.addressLine2,
      line3 = declarantEstablishmentAddress.addressLine3,
      line4 = "", //declarantEstablishmentAddress.addressLine4,
      postcode = declarantEstablishmentAddress.postalCode.getOrElse(fail),
      country = Country(declarantEstablishmentAddress.countryCode)
    ),
    addCompanyDetails = false
  )

  val mrnContactDetailsAnswer: MrnContactDetails = MrnContactDetails(
    fullName = declarantDetails.legalName,
    emailAddress = signedInUserDetails.verifiedEmail,
    phoneNumber = None
  )
  val mrnContactAddressAnswer: ContactAddress    = ContactAddress(
    line1 = declarantEstablishmentAddress.addressLine1,
    line2 = declarantEstablishmentAddress.addressLine2,
    line3 = declarantEstablishmentAddress.addressLine3,
    line4 = "",
    postcode = declarantEstablishmentAddress.postalCode.getOrElse(fail),
    country = Country(declarantEstablishmentAddress.countryCode)
  )

  val filledDraftC285Claim: DraftC285Claim = sample[DraftC285Claim].copy(
    movementReferenceNumber = Some(MovementReferenceNumber(Right(mrn))),
    duplicateMovementReferenceNumberAnswer = None,
    declarationDetailsAnswer = None,
    duplicateDeclarationDetailsAnswer = None,
    declarantTypeAnswer = Some(declarantTypeAnswer),
    detailsRegisteredWithCdsAnswer = None,
    entryNumberContactDetailsAnswer = None,
    mrnContactDetailsAnswer = None,
    mrnContactAddressAnswer = None,
    bankAccountDetailsAnswer = None,
    basisOfClaimAnswer = Some(basisOfClaim),
    supportingEvidencesAnswer = Some(supportingEvidences),
    dutiesSelectedAnswer = Some(completeDutiesSelectedAnswer),
    commoditiesDetailsAnswer = Some(commodityDetailsAnswer),
    reasonForBasisAndClaimAnswer = None,
    claimNorthernIrelandAnswer = Some(northernIrelandAnswer),
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
          declarantDetails = declarantDetails
        )
      )
    ),
    duplicateDisplayDeclaration = None,
    importerEoriNumberAnswer = None,
    declarantEoriNumberAnswer = None,
    claimsAnswer = Some(claimsAnswer),
    scheduledDocumentAnswer = None
  )

  val completeC285Claim: CompleteClaim = CompleteClaim(
    id = filledDraftC285Claim.id,
    movementReferenceNumber = MovementReferenceNumber(Right(mrn)),
    maybeDuplicateMovementReferenceNumberAnswer = None,
    maybeCompleteDeclarationDetailsAnswer = None,
    maybeCompleteDuplicateDeclarationDetailsAnswer = None,
    declarantTypeAnswer = declarantTypeAnswer,
    detailsRegisteredWithCdsAnswer = detailsRegisteredWithCdsFormData,
    maybeContactDetailsAnswer = None,
    maybeBasisOfClaimAnswer = Some(basisOfClaim),
    maybeBankAccountDetailsAnswer = None,
    supportingEvidencesAnswer = supportingEvidences,
    commodityDetailsAnswer = commodityDetailsAnswer,
    northernIrelandAnswer = Some(northernIrelandAnswer),
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
          declarantDetails = declarantDetails
        )
      )
    ),
    maybeDuplicateDisplayDeclaration = None,
    importerEoriNumber = None,
    declarantEoriNumber = None,
    claimsAnswer,
    scheduledDocumentAnswer = None
  )

  "Check Your Answers And Submit Controller" when {

    "handling requests to check all answers" must {

      "redirect to the start of the journey" when {

        "there is no journey status in the session" in {
          val (session, claim, _) = sessionWithCompleteClaimState()

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(journeyStatus = None))
          }

          checkIsRedirect(
            controller.checkAllAnswers(claim.journey)(FakeRequest()),
            baseRoutes.StartController.start()
          )

        }
      }

      "show the confirmation page" in {
        val (session, claim, _) = sessionWithCompleteClaimState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.confirmationOfSubmission(claim.journey)(FakeRequest()),
          messageFromMessageKey("confirmation-of-submission.title")
        )

      }
    }

    "handling requests to submit a claim" when {

      "the submission is a success" must {

        "show the confirmation page" in {
          val draftClaim = filledDraftC285Claim

          val completelyFilledOutClaim =
            sample[FillingOutClaim].copy(signedInUserDetails = signedInUserDetails, draftClaim = draftClaim)

          val (session, claim, _) = sessionWithCompleteClaimState()

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
                submitClaimResponse,
                claim.journey
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
            controller.checkAllAnswersSubmit(claim.journey)(FakeRequest()),
            routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission(claim.journey)
          )

        }

      }

      "the submission is a failure" must {

        "show the submission error page" in {
          val draftClaim = filledDraftC285Claim

          val completelyFilledOutClaim =
            sample[FillingOutClaim].copy(signedInUserDetails = signedInUserDetails, draftClaim = draftClaim)

          val (session, claim, _) = sessionWithCompleteClaimState()

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
                completelyFilledOutClaim.signedInUserDetails,
                claim.journey
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
            controller.checkAllAnswersSubmit(claim.journey)(FakeRequest()),
            routes.CheckYourAnswersAndSubmitController.submissionError(claim.journey)
          )

        }

      }

    }

    "handling requests with a submission error session" must {

      "redirect to the start of the journey" when {

        "the journey is other than a failed submission" in {
          val draftClaim = filledDraftC285Claim

          val completelyFilledOutClaim =
            sample[FillingOutClaim].copy(signedInUserDetails = signedInUserDetails, draftClaim = draftClaim)

          val (session, claim, _) = sessionWithCompleteClaimState()

          val updatedSession = session.copy(journeyStatus = Some(completelyFilledOutClaim))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(
            controller.submissionError(claim.journey)(FakeRequest()),
            baseRoutes.StartController.start()
          )

        }

      }

      "redirect to the confirmation page" when {

        "the claim has just been submitted" in {
          val draftClaim = filledDraftC285Claim

          val completelyFilledOutClaim = sample[FillingOutClaim].copy(draftClaim = draftClaim)

          val (session, claim, _) = sessionWithCompleteClaimState()

          val submitClaimResponse = sample[SubmitClaimResponse]

          val updatedSession = session.copy(journeyStatus = Some(completelyFilledOutClaim))

          val justSubmittedJourney = updatedSession.copy(journeyStatus =
            Some(
              JustSubmittedClaim(
                completelyFilledOutClaim.ggCredId,
                completelyFilledOutClaim.signedInUserDetails,
                completeC285Claim,
                submitClaimResponse,
                claim.journey
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(justSubmittedJourney)
          }

          checkIsRedirect(
            controller.submissionError(claim.journey)(FakeRequest()),
            routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission(claim.journey)
          )

        }

      }

    }

    "show a technical error page" when {

      "the user has not completely filled in the claim" in {
        val draftC285Claim = sample[DraftC285Claim].copy(commoditiesDetailsAnswer = None)

        val fillingOutClaim = sample[FillingOutClaim].copy(draftClaim = draftC285Claim)

        val (session, claim, _) = sessionWithCompleteClaimState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = Some(fillingOutClaim)))
        }

        checkIsTechnicalErrorPage(controller.checkAllAnswers(claim.journey)(FakeRequest()))

      }

      "the submission was a success but the session could not be updated" in {
        val draftClaim = filledDraftC285Claim

        val completelyFilledOutClaim =
          sample[FillingOutClaim].copy(signedInUserDetails = signedInUserDetails, draftClaim = draftClaim)

        val (session, claim, _) = sessionWithCompleteClaimState()

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
              submitClaimResponse,
              claim.journey
            )
          )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
          mockSubmitClaim(submitClaimRequest)(Right(submitClaimResponse))
          mockStoreSession(justSubmittedJourney)(Left(Error("BOOM!")))
        }

        checkIsTechnicalErrorPage(controller.checkAllAnswersSubmit(claim.journey)(FakeRequest()))
      }

    }

    "SubmissionError" should {
      "render the error page when the submission fails" in {
        val journey  = sample[JourneyBindable]
        val ggCredId = sample[GGCredId]
        val email    = sample[Email]
        val eori     = sample[Eori]

        val signedInUserDetails =
          SignedInUserDetails(Some(email), eori, Email("email@email.com"), ContactName("Fred Bread"))

        val journeyStatus = SubmitClaimFailed(ggCredId, signedInUserDetails, journey)
        val session       = SessionData.empty.copy(journeyStatus = Some(journeyStatus))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val page = controller.submissionError(journey)(FakeRequest())
        checkPageIsDisplayed(page, messages("submit-claim-error.title"))

      }
    }
  }

}
