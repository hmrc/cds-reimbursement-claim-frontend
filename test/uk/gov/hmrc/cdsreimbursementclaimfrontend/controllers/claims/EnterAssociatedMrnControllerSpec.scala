package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims


import cats.data.EitherT
import cats.implicits._
import cats.{Functor, Id}
import org.jsoup.nodes.Document
import org.scalatest.OptionValues
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.enterMovementReferenceNumberKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyExtractor, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AssociatedMRNsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{ConsigneeDetails, DisplayDeclaration}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{genOtherThan, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyBindableGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, FeatureSwitchService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterAssociatedMrnControllerSpec extends ControllerSpec  with AuthSupport
  with SessionSupport
  with OptionValues  {

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )


  val controller: EnterMovementReferenceNumberController = instanceOf[EnterMovementReferenceNumberController]
  val featureSwitch: FeatureSwitchService                = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val messageKey: String = "enter-associated-mrn"

    private def sessionWithClaimState(
      maybeAssociatedMRNsAnswer: Option[AssociatedMRNsAnswer],
      movementReferenceNumber: MovementReferenceNumber,
      numberOfClaims: Option[SelectNumberOfClaimsAnswer]
    ): (SessionData, FillingOutClaim, DraftC285Claim) = {
      val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(
        associatedMRNsAnswer = maybeAssociatedMRNsAnswer,
        movementReferenceNumber = Some(movementReferenceNumber),
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

    def getErrorSummary(document: Document): String =
      document.select(".govuk-error-summary__list > li > a").text()




}
