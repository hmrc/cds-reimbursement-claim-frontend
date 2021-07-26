package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.Functor
import cats.Id
import org.scalatest.BeforeAndAfterEach
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaim.PersonalEffects
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutiesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{ConsigneeDetails, ContactDetails, DisplayDeclaration, DisplayResponseDetail, EstablishmentAddress}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BasisOfClaim, DeclarantTypeAnswer, MovementReferenceNumber, SessionData, SignedInUserDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckClaimantDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DetailsRegisteredWithCdsAnswerGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{sample, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._

import scala.concurrent.Future

class CheckClaimantDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TableDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  lazy val controller: CheckClaimantDetailsController = instanceOf[CheckClaimantDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Bulk,
    JourneyBindable.Scheduled
  )

  private def getSessionWithPreviousAnswer(
    displayDeclaration: Option[DisplayDeclaration] = None
  ): (SessionData, FillingOutClaim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(
      displayDeclaration = displayDeclaration
    )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey
    )
  }

  implicit class UpdateSessionWithDeclarantType(sessionData: SessionData) {
    def withDeclarantType(declarantType: DeclarantTypeAnswer): SessionData =
      sessionData.journeyStatus match {
        case Some(FillingOutClaim(g, s, (draftClaim: DraftC285Claim))) =>
          val answer   = Some(declarantType)
          val newClaim = draftClaim.copy(declarantTypeAnswer = answer)
          sessionData.copy(journeyStatus = Some(FillingOutClaim(g, s, newClaim)))
        case _                                                         => fail("Failed to update DeclarantType")
      }
  }

  implicit class UpdateSessionWithAcc14Data(sessionData: SessionData) {
    def withAcc14Data(acc14Response: DisplayResponseDetail): SessionData =
      sessionData.journeyStatus match {
        case Some(FillingOutClaim(g, s, (draftClaim: DraftC285Claim))) =>
          val answer   = Some(DisplayDeclaration(acc14Response))
          val newClaim = draftClaim.copy(displayDeclaration = answer)
          sessionData.copy(journeyStatus = Some(FillingOutClaim(g, s, newClaim)))
        case _                                                         => fail("Failed to update DisplayResponseDetail")
      }
  }

  implicit class ExtractFillingOutClaimClass(sessionData: SessionData) {
    def extractFillingOutClaim():FillingOutClaim = {
      sessionData.journeyStatus match {
        case Some(f@FillingOutClaim(_,_,_)) => f
        case _                                                         => fail("Failed to update DisplayResponseDetail")
      }
    }
  }


  def getEstablishmentAddress(prefix: String): EstablishmentAddress =
    EstablishmentAddress(
      addressLine1 = s"$prefix.addLine1",
      addressLine2 = Some(s"$prefix.addLine2"),
      addressLine3 = Some(s"$prefix.addLine3"),
      postalCode = Some(s"$prefix.pc"),
      countryCode = "GB"
    )

  def getContactDetails(prefix: String): ContactDetails =
    ContactDetails(
      contactName = Some(s"$prefix.JohnSmith"),
      addressLine1 = Some(s"$prefix.addLine1"),
      addressLine2 = Some(s"$prefix.addLine2"),
      addressLine3 = Some(s"$prefix.addLine3"),
      addressLine4 = Some(s"$prefix.addLine4"),
      postalCode = Some(s"$prefix.postalCode"),
      countryCode = Some("GB"),
      telephone = Some(s"$prefix.telephone"),
      emailAddress = Some(s"$prefix.email")
    )


  "CheckClaimantDetailsController" must {

    def performAction(journey: JourneyBindable): Future[Result] = controller.show(journey)(FakeRequest())

    "redirect to the start of the journey" when {
      "there is no journey status in the session" in forAll(journeys) { journey =>
        val session = getSessionWithPreviousAnswer(None)._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(journey),
          baseRoutes.StartController.start()
        )
      }
    }

    "display the page" in forAll(journeys) { journey =>
      val displayDeclaration = sample[DisplayDeclaration]
      val session            = getSessionWithPreviousAnswer(Some(displayDeclaration))._1

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(journey),
        messageFromMessageKey("claimant-details.title"),
        doc => {
          println(doc)
          //doc.select("")
        }
      )
    }
  }

  "CheckClaimantDetailsController Companion object" should {
    "extract contact details for Importer" in {
      val contactDetails       = getContactDetails("acc14.cons.cont")
      val establishmentAddress = getEstablishmentAddress("acc14.cons.est")
      val consignee            = sample[ConsigneeDetails]
        .copy(establishmentAddress = establishmentAddress, contactDetails = Some(contactDetails))
      val displayDeclaration        = Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consignee))))
      val (session,fillingOutClaim)            = getSessionWithPreviousAnswer(Some(displayDeclaration))

        val foc = session
          .withDeclarantType(DeclarantTypeAnswer.AssociatedWithRepresentativeCompany)
        .withAcc14Data(displayDeclaration.displayResponseDetail)
          .extractFillingOutClaim

      val res = extractContactsRegisteredWithCDSA(foc)
      //println(res)
    }
  }

}
